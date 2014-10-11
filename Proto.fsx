open System
open System.Threading

type Atom<'a when 'a : not struct>(value : 'a) =
    let r = ref value
    let watches = ref Map.empty

    let rec compareAndSwap r f =
        let oldVal = !r
        let newVal = f oldVal
        let result = Interlocked.CompareExchange<_>(r, newVal, oldVal)
        if Object.ReferenceEquals(result, oldVal) then (oldVal, newVal)
        else compareAndSwap r f

    member x.get () = !r
    member x.swap f =
        let oldValue, newValue = compareAndSwap r f
        Map.iter (fun key watch -> watch key x oldValue newValue) !watches
        newValue

    member x.getWatches () = !watches
    member x.addWatch key watch =
        compareAndSwap watches (fun w -> Map.add key watch w) |> ignore
        x
    member x.removeWatch key =
        compareAndSwap watches (fun w -> Map.remove key w) |> ignore
        x

let atom value = new Atom<_>(value)


