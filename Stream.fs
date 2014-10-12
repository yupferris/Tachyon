module Tachyon.Stream
    open Tachyon.IStream
    open Tachyon.Atom

    type EventStream<'a>() =
        let e = new Event<'a>()
        let p = e.Publish

        interface IStream<'a> with
            member x.addWatch h = lock e (fun () -> p.AddHandler h)
            member x.removeWatch h = lock e (fun () -> p.RemoveHandler h)

        member x.trigger v = lock e (fun () -> e.Trigger v)

    let stream (e : Event<_>) =
        let p = e.Publish
        {
            new IStream<'b> with
                member x.addWatch h = lock e (fun () -> p.AddHandler h)
                member x.removeWatch h = lock e (fun () -> p.RemoveHandler h)
        }

    let buildEventStream (a : IStream<'a>) f =
        let s = new EventStream<'b>()
        a.addWatch (new Handler<_>(fun _ x -> f (s.trigger) x))
        s :> IStream<_>

    let map f a = buildEventStream a (fun t x -> t (f x))

    let filter f a = buildEventStream a (fun t x -> if f x then t x)

    let choose f a = buildEventStream a (fun t x ->
        match f x with
        | Some x -> t x
        | _ -> ())

    let foldp f b a =
        let id = atom b
        buildEventStream a (fun t x -> t (id.swap (fun y -> f x y)))

    let subscribe f (a : IStream<_>) =
        a.addWatch (new Handler<_>(fun _ x -> f x))
        a
