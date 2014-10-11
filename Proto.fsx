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
    let watches = new AtomicReference<Map<'b, 'a -> 'a -> unit>>(Map.empty)

    member x.get = r.get
    member x.swap f =
        let rec aux () =
            let oldValue = r.get()
            let newValue = f oldValue
            if r.compareAndSwap oldValue newValue then
                Map.iter (fun key watch -> watch oldValue newValue) (watches.get())
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

let atom value = new Atom<_, int>(value)

let map<'a, 'b when 'a : not struct and 'b : not struct> f (a : Atom<'a, _>) =
    let ret = atom(f (a.get()))
    a.addWatch 0 (fun _ x -> ret.swap(fun _ -> f x) |> ignore) |> ignore
    ret

let a = atom "hasn't updated yet"
let b = map (fun (x : string) -> sprintf "the length of the last value was %d" x.Length) a
b.get()
