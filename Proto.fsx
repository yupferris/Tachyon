open System
open System.Threading

type AtomicReference<'a when 'a : not struct> (value : 'a) =
    let r = ref value

    member x.get () = !r
    member x.compareAndSwap oldValue newValue =
        let result = Interlocked.CompareExchange<_>(r, newValue, oldValue)
        obj.ReferenceEquals(result, oldValue)
    member x.swap f =
        let rec aux () =
            let oldValue = !r
            let newValue = f oldValue
            if x.compareAndSwap oldValue newValue then newValue
            else aux()
        aux()

type Atom<'a when 'a : not struct> (value : 'a) =
    let r = new AtomicReference<'a>(value)
    let watches = new AtomicReference<('a -> 'a -> unit) list>([])

    member x.get = r.get
    member x.swap f =
        let rec aux () =
            let oldValue = r.get()
            let newValue = f oldValue
            if r.compareAndSwap oldValue newValue then
                List.iter (fun watch -> watch oldValue newValue) (watches.get())
                newValue
            else aux()
        aux()

    member x.getWatches = watches.get
    member x.addWatch watch =
        watches.swap (fun w -> watch :: w) |> ignore
        x

let atom value = new Atom<_>(value)

let map<'a, 'b when 'a : not struct and 'b : not struct> (f : 'a -> 'b) (a : Atom<'a>) =
    let ret = atom(f (a.get()))
    a.addWatch (fun _ x -> ret.swap(fun _ -> f x) |> ignore) |> ignore
    ret

let filter f = map (fun x -> if f x then x else Unchecked.defaultof<_>)

let choose f =
    map
        (fun x ->
            match f x with
            | Some x -> x
            | _ -> Unchecked.defaultof<_>)

let foldp<'a, 'b when 'a : not struct and 'b : not struct> (f : 'a -> 'b -> 'b) (b : 'b) (a : Atom<'a>) =
    let ret = atom(f (a.get()) b)
    a.addWatch (fun _ x -> ret.swap(fun p -> f x p) |> ignore) |> ignore
    ret

let a = atom ""
let b = foldp (fun x y -> y + " " + x) "" a

Array.iter (fun x -> a.swap (fun _ -> x) |> ignore) ("these are some words".Split(' '))
b.get()
