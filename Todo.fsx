#load "IStream.fs"
#load "Atom.fs"
#load "Stream.fs"

open System
open Tachyon.IStream
open Tachyon.Stream

type SimpleStream<'a>() =
    let e = new Event<'a>()
    let p = e.Publish

    interface IStream<'a> with
        member x.addWatch h = lock e (fun () -> p.AddHandler h)
        member x.removeWatch h = lock e (fun () -> p.AddHandler h)

    member x.trigger v = e.Trigger v

type Todo =
    {
        description : string
        id : int
    }

type Action =
    | Add of string
    | Remove of int

type State =
    {
        items : Map<int, Todo>
        nextId : int
    }
    with static member empty = { items = Map.empty; nextId = 0 }

let apply state = function
    | Add x ->
        let item =
            {
                description = x
                id = state.nextId
            }
        {
            items = Map.add item.id item state.items
            nextId = item.id + 1
        }
    | Remove x ->
        {
            state with
                items = Map.remove x state.items
        }

let actionStream = new SimpleStream<Action>()

let count =
    actionStream
    |> map (fun _ -> 1)
    |> scan (+) 0

let additions =
    actionStream
    |> filter (fun x ->
        match x with
        | Add _ -> true
        | _ -> false)
    |> scan (fun x y -> y :: x) []
    |> map List.rev

let removals =
    actionStream
    |> filter (fun x ->
        match x with
        | Remove _ -> true
        | _ -> false)
    |> scan (fun x y -> y :: x) []
    |> map List.rev

let state =
    actionStream
    |> scan apply State.empty

let statePrinter =
    state
    |> subscribe (printfn "State updated: %A")

[
    Add "Gotta do that"
    Add "And that"
    Add "Don't forget that"
    Remove 1
    Add "Oh wait this too"
    Remove 2
    Add "Cool, just this thing as well"
    Remove 0
    Remove 3
    Remove 4
    Add "All done, but let's put this in there too for funsies"
]
|> List.iter (actionStream.trigger)
