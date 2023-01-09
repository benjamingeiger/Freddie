open System

open Freddie.Shared

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
        "My dreams were ice. A life unfulfilled; wakin' everybody up, taking booze and pills"
        "Let your life be pain with joy between lovers without fear"
    ] |> String.concat "\n"

[<EntryPoint>]
let main argv =
    let result = Parser.parse testInput
    match result with
    | Result.Ok result' -> List.iter (printfn "%A") result'
    | Result.Error error -> failwithf "Parse failed: %s" error

    // let result = Parser.parseTest "pain without fear"
    // printfn "%A" result

    0
