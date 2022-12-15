namespace Interpreter

module Parser =
    open FParsec

    let parseProgram = pstring "foo"

    let parse lines =
        lines
        |> String.concat "\n"
        |> runParserOnString parseProgram () ""
