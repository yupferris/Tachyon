module Tachyon.Atom
    type Atom<'a>(value : 'a) =
        let r = ref value

        member x.get () = !r
        member x.swap f = lock r (fun () ->
            let oldValue = !r
            let newValue = f oldValue
            r := newValue
            newValue)

    let atom value = new Atom<_>(value)
