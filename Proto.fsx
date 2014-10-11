open System
open System.Threading

type AtomicReference<'a when 'a : not struct> (value : 'a) =
    let r = ref value

    member x.get () = !r
    member x.compareAndSwap oldValue newValue =
        let result = Interlocked.CompareExchange<_>(r, newValue, oldValue)
        Object.ReferenceEquals(result, oldValue)
    member x.swap f =
        let rec aux () =
            let oldValue = !r
            let newValue = f oldValue
            if x.compareAndSwap oldValue newValue then newValue
            else aux()
        aux()

type Atom<'a, 'b when 'a : not struct and 'b : comparison> (value : 'a) =
    let r = new AtomicReference<'a>(value)
    let watches = new AtomicReference<Map<'b, 'b -> Atom<'a, 'b> -> 'a -> 'a -> unit>>(Map.empty)

    member x.get = r.get
    member x.swap f =
        let rec aux () =
            let oldValue = r.get()
            let newValue = f oldValue
            if r.compareAndSwap oldValue newValue then
                Map.iter (fun key watch -> watch key x oldValue newValue) (watches.get())
                newValue
            else aux()
        aux()

    member x.getWatches = watches.get
    member x.addWatch key watch =
        watches.swap (fun w -> Map.add key watch w) |> ignore
        x
    member x.removeWatch key =
        watches.swap (fun w -> Map.remove key w) |> ignore
        x

    interface IComparable
        with member x.CompareTo other = 1 // lol

let atom value = new Atom<_, _>(value)

let map<'a, 'b when 'a : not struct and 'b : not struct> f (a : Atom<'a, _>) =
    let ret = new Atom<'b, _>(f (a.get()))
    a.addWatch ret (fun _ _ _ x -> ret.swap(fun _ -> f x) |> ignore) |> ignore
    ret

let a = atom "hasn't updated yet"
//let b = map (fun _ -> printfn "YEP IT'S WOOD") a
