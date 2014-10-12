module Tachyon.Identity
    open Tachyon.Atom
    open Tachyon.Stream

    type IIdentity<'a> =
        inherit IStream<'a>

        abstract get : unit -> 'a
        abstract swap : ('a -> 'a) -> 'a

    type Identity<'a>(v : Atom<'a>) =
        let e = new Event<'a>()
        let p = e.Publish

        interface IIdentity<'a> with
            member x.get () = v.get()
            member x.swap f = v.swap f

            member x.addWatch h = v.addWatch h
            member x.removeWatch h = v.removeWatch h

    let identity x = new Identity<_>(x) :> IIdentity<_>
