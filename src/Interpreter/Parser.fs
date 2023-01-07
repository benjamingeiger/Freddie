namespace Interpreter

open FParsec

module Parser =

    let toLower (s: string) = if isNull s then s else s.ToLowerInvariant ()

    // ======================================================================
    // Keyword Lists
    // ======================================================================

    let pronounList = [
        "he"; "her"; "him"; "hir"; "it"; "she"; "them"; "they"; "ve"; "ver";
        "xe"; "xem"; "ze"; "zie"; "zir"
    ]
    let commonPrefixList = [ "a"; "an"; "my"; "our"; "the"; "your" ]
    let literalList = [
        "definitely"; "empty"; "false"; "false"; "gone"; "lies"; "maybe";
        "mysterious"; "no"; "nobody"; "nothing"; "nowhere"; "null"; "ok";
        "silence"; "silent"; "true"; "wrong"; "yes";
    ]
    let assignmentList = [
        "are"; "be"; "in"; "into"; "is"; "let"; "put"; "said"; "say"; "says";
        "was"; "were"
    ]
    let operationList = [
        "and"; "around"; "at"; "between"; "build"; "burn"; "cast"; "cut";
        "down"; "into"; "join"; "knock"; "like"; "minus"; "nor"; "not"; "of";
        "or"; "over"; "plus"; "pop"; "push"; "rock"; "roll"; "round";
        "shatter"; "split"; "times"; "turn"; "unite"; "up"; "with"; "without"
    ]
    let comparisonList = [
        "ain't"; "aint"; "aren't"; "arent"; "as"; "big"; "bigger"; "great";
        "greater"; "high"; "higher"; "is"; "isn't"; "isnt"; "less"; "little";
        "low"; "lower"; "not"; "small"; "smaller"; "strong"; "stronger";
        "than"; "wasn't"; "wasnt"; "weak"; "weaker"; "weren't"; "werent"
    ]
    let ioList = [ "listen"; "say"; "scream"; "shout"; "to"; "whisper" ]
    let controlFlowList = [
        "break"; "continue"; "down"; "else"; "if"; "it"; "take"; "the"; "to";
        "top"; "until"; "while"
    ]
    let functionList = [
        "back"; "give"; "return"; "send"; "takes"; "taking"; "wants"
    ]
    let keywordList = List.concat [
        pronounList
        commonPrefixList
        literalList
        assignmentList
        operationList
        comparisonList
        ioList
        controlFlowList
        functionList
    ]

    // I borrowed a part of this from the FParsec documentation. -- bgeiger, 2023-01-05
    let isNotKeyword (p : Parser<_, unit>) : Parser<_, unit> =
        fun (stream : CharStream<unit>) ->
            let state = stream.State
            let reply = p stream
            if reply.Status <> Ok || List.contains (toLower reply.Result) keywordList then
                reply
            else
                stream.BacktrackTo state
                Reply(Error, messageError "keyword values are not valid here")


    // ======================================================================
    // AST
    // ======================================================================

    type Identifier = Identifier of string

    and Expression =
        | StringLiteral of string
        | NumericLiteral of double
        | BooleanLiteral of bool

    and Statement =
        | Output of Expression

    and Block = Block of Statement list


    // ======================================================================
    // Parsers
    // ======================================================================

    let uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĸĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"
    let lowercaseLetters = "abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþāăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷÿźżžŉß"
    let letters = uppercaseLetters + lowercaseLetters
    let (word : Parser<_, unit>) = many1Chars (anyOf letters)

    // ----------------------------------------------------------------------
    // Markup details (comments, whitespace)
    // ----------------------------------------------------------------------

    let noiseChars = ";.?!&."

    let comment = (skipChar '(') >>. (skipCharsTillString ")" true System.Int32.MaxValue)
    let whitespace = many1Chars (anyOf " \t") |>> ignore
    let __ = many (whitespace <|> comment) |>> ignore
    let __' = notEmpty __

    let noise = many (__' <|> skipAnyOf noiseChars)

    let EOL = noise >>. skipNewline
    let EOF = noise >>. eof

    // Forward reference for statements
    let statement, statementRef = createParserForwardedToRef<Statement, unit>()

    let (stringLiteral : Parser<Expression, unit>) = skipChar '"' >>. (manyChars (noneOf "\n\"")) .>> skipChar '"' |>> StringLiteral
    let emptyString = ["empty"; "silent"; "silence"] |> List.map pstringCI |> choice >>% StringLiteral ""

    let trueConstant = ["true"; "ok"; "right"; "yes"] |> List.map pstringCI |> choice >>% BooleanLiteral true
    let falseConstant = ["false"; "lies"; "wrong"; "no"] |> List.map pstringCI |> choice >>% BooleanLiteral false

    // I know I should probably use FParsec's pfloat for this but it handles
    // things that Rockstar doesn't support and vice versa. -- bgeiger, 2023-01-06
    let (numericLiteralFractionalPart : Parser<_, unit>)  = manyChars2 (pchar '.') digit
    let numericLiteral =
        notEmpty ((opt (pchar '-')) .>>. manyChars digit .>>. numericLiteralFractionalPart)
        |>> fun ((signChar, wholeDigits), fractional) ->
            (sprintf "%c%s%s" (Option.defaultValue ' ' signChar) wholeDigits fractional |> float |> NumericLiteral)

    let expression = choice [
        stringLiteral
        emptyString
        trueConstant
        falseConstant
        numericLiteral
    ]

    let output = (["say"; "shout"; "whisper"; "scream"] |> List.map pstringCI |> choice) >>. __ >>. expression |>> Output

    do statementRef.Value <- __ >>. choice [ output ]
    let line = __ >>. statement .>> (EOF <|> (skipMany1 EOL))
    let program = many line

    let parse source = runParserOnString program () "" source |> function
        | Success (result, _, _) -> Result.Ok result
        | Failure (errorAsString, _, _) -> Result.Error errorAsString
