open System
open System.Threading

type AtomicReference<'a when 'a : not struct> (value : 'a) =
    let r = ref value

    member x.get () = !r
    member x.compareAndSwap oldValue newValue =
        let result = Interlocked.CompareExchange<_>(r, newValue, oldValue)
        obj.ReferenceEquals(result, oldValue)
    member x.swap f =
        let rec aux () =
            let oldValue = !r
            let newValue = f oldValue
            if x.compareAndSwap oldValue newValue then newValue
            else aux()
        aux()

type Atom<'a when 'a : not struct> (value : 'a) =
    let r = new AtomicReference<'a>(value)
    let watches = new AtomicReference<('a -> 'a -> unit) list>([])

    member x.get = r.get
    member x.swap f =
        let rec aux () =
            let oldValue = r.get()
            let newValue = f oldValue
            if r.compareAndSwap oldValue newValue then
                List.iter (fun watch -> watch oldValue newValue) (watches.get())
                newValue
            else aux()
        aux()

    member x.getWatches = watches.get
    member x.addWatch watch =
        watches.swap (fun w -> watch :: w) |> ignore
        x

let atom value = new Atom<_>(value)

let map<'a, 'b when 'a : not struct and 'b : not struct> (f : 'a -> 'b) (a : Atom<'a>) =
    let ret = atom(f (a.get()))
    a.addWatch (fun _ x -> ret.swap(fun _ -> f x) |> ignore) |> ignore
    ret

let filter<'a when 'a : not struct> (f : 'a -> bool) (a : Atom<'a>) =
    let seed = a.get()
    let ret = atom(if f seed then seed else Unchecked.defaultof<_>)
    a.addWatch (fun _ x -> if f x then ret.swap(fun _ -> x) |> ignore) |> ignore
    ret

let choose<'a, 'b when 'a : not struct and 'b : not struct> (f : 'a -> 'b option) (a : Atom<'a>) =
    let seed = a.get()
    let ret =
        atom(
            match f seed with
            | Some x -> x
            | _ -> Unchecked.defaultof<_>)
    a.addWatch
        (fun _ x ->
            match f x with
            | Some x -> ret.swap(fun _ -> x) |> ignore
            | _ -> ()) |> ignore
    ret

let foldp<'a, 'b when 'a : not struct and 'b : not struct> (f : 'a -> 'b -> 'b) (b : 'b) (a : Atom<'a>) =
    let ret = atom(f (a.get()) b)
    a.addWatch (fun _ x -> ret.swap(fun p -> f x p) |> ignore) |> ignore
    ret

let a = atom ""
let b =
    a
    |> filter (String.IsNullOrEmpty >> not)
    |> foldp (fun x y -> y + " " + x) ""

Array.iter (fun x -> a.swap (fun _ -> x) |> ignore) ("these are some words".Split(' '))
b.get()
