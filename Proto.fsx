#load "Atom.fs"
#load "Stream.fs"
#load "Identity.fs"

open System
open Tachyon.Atom
open Tachyon.Stream
open Tachyon.Identity

let a = atom ""
let b =
    a
    |> identity
    |> filter (String.IsNullOrEmpty >> not)
    |> choose (fun x -> if not (List.exists ((=) x) ["duck"; "horse"]) then Some x else None)
    |> foldp
        (fun x y ->
            if String.IsNullOrEmpty x then y
            else if String.IsNullOrEmpty y then x
            else y + " " + x) ""
    |> subscribe (printfn "%A")

Array.iter (fun x -> a.swap (fun _ -> x) |> ignore) ("these duck are some horse words".Split(' '))
