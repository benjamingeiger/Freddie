open System

open Interpreter

[<EntryPoint>]
let main argv =
    let result = Parser.parse ["foo"]
    printfn "%A" result

    0
