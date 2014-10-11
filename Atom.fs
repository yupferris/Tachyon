module Tachyon.Atom
    open System
    open System.Threading

    type Atom<'a when 'a : not struct> =
        {
            get : unit -> 'a
            swap : ('a -> 'a) -> 'a
        }

    let atom obj =
        let r = ref obj

        let get () = !r
        let rec swap f =
            let oldVal = !r
            let newVal = f oldVal
            let result = Interlocked.CompareExchange<_>(r, newVal, oldVal)
            if Object.ReferenceEquals(result, oldVal) then newVal
            else swap f

        {
            get = get
            swap = swap
        }
