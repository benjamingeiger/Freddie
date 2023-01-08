namespace Freddie.Shared

open FParsec

open Freddie.Shared.AST

module Parser =

    let toLower (s: string) = if isNull s then s else s.ToLowerInvariant ()

    // ======================================================================
    // Keyword Lists
    // ======================================================================

    let pronounList =
        [
            "he"; "her"; "him"; "hir"; "it"; "she"; "them"; "they"; "ve"; "ver";
            "xe"; "xem"; "ze"; "zie"; "zir"
        ] |> List.sortByDescending String.length
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
            if reply.Status <> Ok then
                reply
            elif List.contains (toLower reply.Result) keywordList then
                // printfn "Keyword: %A" reply.Result
                stream.BacktrackTo state
                Reply(Error, messageError "keyword values are not valid here")
            else
                reply

    // debugging operator borrowed from the FParsec docs. -- bgeiger, 2023-01-07
    // let ( **-> ) label (p: Parser<_,_>) : Parser<_,_> =
        // fun stream ->
            // printfn "%A: Entering %s" stream.Position label
            // let reply = p stream
            // printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            // reply

    let ( **-> ) _ (p: Parser<_,_>) : Parser<_,_> = p

    let keyword (keywords : string list) : Parser<string, unit> =
        keywords
        |> List.sortByDescending String.length
        |> List.map (pstringCI)
        |> choice

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

    let comment = "comment" **-> (skipChar '(') >>. (skipCharsTillString ")" true System.Int32.MaxValue)
    let whitespace = "whitespace" **-> skipMany1 (anyOf " \t")
    let __ = "__" **-> skipMany (whitespace <|> comment)
    let __' = "__'" **-> notEmpty __

    let noise = "noise" **-> skipMany (__' <|> skipAnyOf ";,.?!&i'")

    let EOL = "EOL" **-> (opt noise) >>. skipNewline

    // Forward reference for statements
    let statement, statementRef = createParserForwardedToRef<Statement, unit>()

    let (stringLiteral : Parser<Expression, unit>) = "stringLiteral" **-> skipChar '"' >>. (manyChars (noneOf "\n\"")) .>> skipChar '"' |>> StringLiteral

    let trueConstant = "trueConstant" **-> keyword ["true"; "ok"; "right"; "yes"] >>% BooleanLiteral true
    let falseConstant = "falseConstant" **-> keyword ["false"; "lies"; "wrong"; "no"] >>% BooleanLiteral false

    // I know I should probably use FParsec's pfloat for this but it handles
    // things that Rockstar doesn't support and vice versa. -- bgeiger, 2023-01-06
    let (numericLiteralFractionalPart : Parser<_, unit>)  = manyChars2 (pchar '.') digit
    let numericLiteral = 
        "numericLiteral" **->
        (notEmpty ((opt (pchar '-')) .>>. manyChars digit .>>. numericLiteralFractionalPart)
            |>> fun ((signChar, wholeDigits), fractional) ->
                (sprintf "%c%s%s" (Option.defaultValue ' ' signChar) wholeDigits fractional |> float |> NumericLiteral))

    // note to self: this is probably going to cause ordering issues, between assignment and variable definitions. -- bgeiger, 2023-01-07
    let is = "is" **-> skipStringCI "'s" <|> skipStringCI "'re" <|> (__ >>. (keyword ["is"; "was"; "are"; "were"] |>> ignore))

    let name = "name" **-> many1Chars (anyOf (letters + "'")) |>> fun (s : string) -> s.Replace("'", "")
    let properNoun = "properNoun" **-> isNotKeyword (many1Chars2 (anyOf uppercaseLetters) (anyOf (letters + "'")))
    let properVariable = "properVariable" **-> sepBy1 properNoun (skipMany1 (pchar ' ')) |>> fun names -> names |> List.map toLower |> String.concat " " |> Variable
    let commonPrefix = "commonPrefix" **-> keyword commonPrefixList
    let commonVariable = "commonVariable" **-> (commonPrefix .>> __') .>>. isNotKeyword name |>> fun (prefix, name') -> Variable (toLower prefix + " " + toLower name')
    let simpleVariable = "simpleVariable" **-> isNotKeyword name |>> (toLower >> Variable)
    let pronoun = "pronoun" **-> ((keyword pronounList) .>> followedBy (is <|> __ <|> EOL <|> eof) >>% Pronoun)

    let poeticEmptyString = "poeticEmptyString" **-> keyword ["empty"; "silent"; "silence"] >>% StringLiteral ""

    let expression = "expression" **-> (choice [
        stringLiteral
        trueConstant
        falseConstant
        numericLiteral
        attempt pronoun
        properVariable
        commonVariable
        simpleVariable
        poeticEmptyString ])

    let output = "output" **-> (keyword ["say"; "shout"; "whisper"; "scream"]) >>. __' >>. expression |>> Output
    let (nullStatement : Parser<_, unit>) = "nullStatement" **-> preturn Null

    do statementRef.Value <- "statement" **-> __ >>. choice [ output; nullStatement ]
    let line = "line" **-> __ >>. statement .>> noise
    let program = "program" **-> sepBy line EOL .>> eof

    let parse source = runParserOnString program () "" source |> function
        | Success (result, _, _) -> Result.Ok result
        | Failure (errorAsString, _, _) -> Result.Error errorAsString
