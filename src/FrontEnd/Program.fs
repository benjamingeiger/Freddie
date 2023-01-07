open System

open Interpreter

// let testInput =
    // [
        // "foo"
        // "    foo"
        // "\tfoo"
        // "foo   "
        // "   foo   "
        // "foo (bar)"
        // "(bar) foo"
        // "   (bar)   foo"
    // ] |> String.concat "\n"

// let testInput =
    // [
        // "break"
        // "break,"
        // "    break! (one nine!)"
        // "(if you) continue (your heart)"
    // ] |> String.concat "\n"

let testInput =
    [
        "shout \"herp derp\""
    ] |> String.concat "\n"

[<EntryPoint>]
let main argv =
    let result = Parser.parse testInput
    printfn "%A" result

    0
