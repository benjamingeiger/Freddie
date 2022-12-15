open System

open Interpreter

let testInput1 =
    [
        "foo"
        "    foo"
        "\tfoo"
        "foo   "
        "   foo   "
        "foo (bar)"
        "(bar) foo"
        "   (bar)   foo"
    ] |> String.concat "\n"

[<EntryPoint>]
let main argv =
    let result = Parser.parse testInput1
    printfn "%A" result

    0
