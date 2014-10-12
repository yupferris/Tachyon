module Tachyon.Atom
    open Tachyon.IStream

    type Atom<'a>(value : 'a) =
        let r = ref value

        let e = new Event<'a>()
        let p = e.Publish

        member x.get () = !r
        member x.swap f =
            let newValue = lock r (fun () ->
                let oldValue = !r
                let newValue = f oldValue
                r := newValue
                newValue)
            lock e (fun () -> e.Trigger newValue)
            newValue

        interface IStream<'a> with
            member x.addWatch h = lock e (fun () -> p.AddHandler h)
            member x.removeWatch h = lock e (fun () -> p.RemoveHandler h)

    let atom value = new Atom<_>(value)
