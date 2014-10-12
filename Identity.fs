module Tachyon.Identity
    open Tachyon.Atom
    open Tachyon.Stream

    type IIdentity<'a> =
        inherit IStream<'a>

        abstract get : unit -> 'a
        abstract swap : ('a -> 'a) -> 'a

    type Identity<'a>(value : 'a) =
        let v = atom value

        let e = new Event<'a>()
        let p = e.Publish

        interface IIdentity<'a> with
            member x.get () = v.get()
            member x.swap f =
                let newValue = v.swap f
                lock e (fun () -> e.Trigger newValue)
                newValue

            member x.addWatch h = lock e (fun () -> p.AddHandler h)
            member x.removeWatch h = lock e (fun () -> p.RemoveHandler h)

    let identity x = new Identity<_>(x) :> IIdentity<_>
