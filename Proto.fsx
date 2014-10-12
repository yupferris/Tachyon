#load "IStream.fs"
#load "Atom.fs"
#load "Stream.fs"

open System
open Tachyon.Atom
open Tachyon.Stream

let a = atom ""
let b =
    a
    |> filter (String.IsNullOrEmpty >> not)
    |> choose (fun x -> if not (List.exists ((=) x) ["duck"; "horse"]) then Some x else None)
    |> scan
        (fun x y ->
            if String.IsNullOrEmpty x then y
            else if String.IsNullOrEmpty y then x
            else x + " " + y) ""
    |> subscribe (printfn "%A")

"these duck are some horse words".Split(' ')
|> Array.map (fun x -> async { a.swap (fun _ -> x) |> ignore })
|> Async.Parallel
|> Async.RunSynchronously
