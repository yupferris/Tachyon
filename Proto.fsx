open System

type IStream<'a> =
    abstract addWatch : Handler<'a> -> unit
    abstract removeWatch : Handler<'a> -> unit

type IIdentity<'a> =
    inherit IStream<'a>

    abstract get : unit -> 'a
    abstract swap : ('a -> 'a) -> 'a

type Identity<'a>(value : 'a) =
    let v = ref value

    let e = new Event<'a>()
    let p = e.Publish

    interface IIdentity<'a> with
        member x.get () = !v
        member x.swap f = lock v (fun () ->
            let oldValue = !v
            let newValue = f oldValue
            v := newValue
            e.Trigger newValue
            newValue)

        member x.addWatch h = lock e (fun () -> p.AddHandler h)
        member x.removeWatch h = lock e (fun () -> p.RemoveHandler h)

let stream (e : Event<_>) =
    let p = e.Publish
    {
        new IStream<'b> with
            member x.addWatch h = lock e (fun () -> p.AddHandler h)
            member x.removeWatch h = lock e (fun () -> p.RemoveHandler h)
    }

let identity x = new Identity<_>(x) :> IIdentity<_>

let map<'a, 'b> f (a : IStream<'a>) =
    let e = new Event<'b>()
    a.addWatch (new Handler<_>(fun _ x -> e.Trigger (f x)))
    stream e

let filter f (a : IStream<_>) =
    let e = new Event<_>()
    a.addWatch (new Handler<_>(fun _ x -> if f x then e.Trigger x))
    stream e

let choose<'a, 'b> f (a : IStream<'a>) =
    let e = new Event<'b>()
    a.addWatch (new Handler<_>(fun _ x ->
        match f x with
        | Some x -> e.Trigger x
        | _ -> ()))
    stream e

let foldp<'a, 'b> f b (a : IStream<'a>) =
    let id = identity b
    let e = new Event<'b>()
    a.addWatch (new Handler<_>(fun _ x -> e.Trigger (id.swap (fun _ -> f x (id.get())))))
    stream e

let subscribe f (a : IStream<_>) =
    a.addWatch (new Handler<_>(fun _ x -> f x))
    a

let a = identity ""
let b =
    a
    |> filter (String.IsNullOrEmpty >> not)
    |> choose (fun x -> if not (List.exists ((=) x) ["duck"; "horse"]) then Some x else None)
    |> foldp
        (fun x y ->
            if String.IsNullOrEmpty x then y
            else if String.IsNullOrEmpty y then x
            else y + " " + x) ""
    |> subscribe (printfn "%A")

Array.iter (fun x -> a.swap (fun _ -> x) |> ignore) ("these duck are some horse words".Split(' '))
