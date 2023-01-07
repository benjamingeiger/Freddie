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
        "shout \"herp derp\""
        "whisper ok"
        "say lies"
        "scream silence"
        "shout 0.123"
        "shout .456"
        "shout 789"
    ] |> String.concat "\n"

[<EntryPoint>]
let main argv =
    let result = Parser.parse testInput
    match result with
    | Result.Ok result' -> List.iter (printfn "%A") result'
    | Result.Error error -> failwithf "Parse failed: %s" error

    0
