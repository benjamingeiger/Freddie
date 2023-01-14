module Freddie.Shared.Parser

open FParsec
open Freddie.Shared.AST

module Parsers =

    let toLower (s: string) = if isNull s then s else s.ToLowerInvariant ()

    // ======================================================================
    // Keyword Lists
    // ======================================================================

    let pronounList =
        [
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
            if reply.Status <> Ok then
                reply
            elif List.contains (toLower reply.Result) keywordList then
                // printfn "Keyword: %A" reply.Result
                stream.BacktrackTo state
                Reply(Error, messageError "keyword values are not valid here")
            else
                reply

    // debugging operator borrowed from the FParsec docs. -- bgeiger, 2023-01-07
    let ( **-> ) label (p: Parser<_,_>) : Parser<_,_> =
        // let DEBUG = true
        let DEBUG = false
        if DEBUG then
            fun stream ->
                printfn "%A: Entering %s" stream.Position label
                let reply = p stream
                printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
                reply
        else p


    let keyword (keywords : string list) : Parser<string, unit> =
        keywords
        |> List.sortByDescending String.length
        |> List.map (pstringCI)
        |> choice

    let sepBy' p sep = pipe2 p (many (sep >>. p)) (fun hd tl -> hd::tl) <|>% []

    // ======================================================================
    // Parsers
    // ======================================================================

    let uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĸĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"
    let lowercaseLetters = "abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþāăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷÿźżžŉß"
    let letters = uppercaseLetters + lowercaseLetters
    let noiseChars = ";,.?!&'"
    let poeticDigitSeparators = " \t0123456789',;:?!+_/"

    // ----------------------------------------------------------------------
    // Markup details (comments, whitespace)
    // ----------------------------------------------------------------------

    let (word : Parser<_, unit>) = many1Chars (anyOf letters)

    let comment = "comment" **-> (skipChar '(') >>. (skipCharsTillString ")" true System.Int32.MaxValue)
    let whitespace = "whitespace" **-> skipMany1 (anyOf " \t")
    let __' = "__'" **-> skipMany (whitespace <|> comment)
    let __ = "__" **-> notEmpty __'

    let noise = "noise" **-> skipMany (__ <|> skipAnyOf noiseChars)

    let EOL = "EOL" **-> (opt noise) >>. skipNewline

    // Forward references
    let statement, statementRef = createParserForwardedToRef<Statement, unit>()
    let expression, expressionRef = createParserForwardedToRef<Expression, unit>()

    let (stringLiteral : Parser<Expression, unit>) = "stringLiteral" **-> skipChar '"' >>. (manyChars (noneOf "\n\"")) .>> skipChar '"' |>> StringValue

    let trueConstant = "trueConstant" **-> keyword ["true"; "ok"; "right"; "yes"] >>% BooleanValue true
    let falseConstant = "falseConstant" **-> keyword ["false"; "lies"; "wrong"; "no"] >>% BooleanValue false

    // I know I should probably use FParsec's pfloat for this but it handles
    // things that Rockstar doesn't support and vice versa. -- bgeiger, 2023-01-06
    let (numericLiteralFractionalPart : Parser<_, unit>)  = manyChars2 (pchar '.') digit
    let numericLiteral = 
        "numericLiteral" **->
        (notEmpty ((opt (pchar '-')) .>>. manyChars digit .>>. numericLiteralFractionalPart)
            |>> fun ((signChar, wholeDigits), fractional) ->
                (sprintf "%c%s%s" (Option.defaultValue ' ' signChar) wholeDigits fractional |> float |> NumericValue))

    // note to self: this is probably going to cause ordering issues, between assignment and variable definitions. -- bgeiger, 2023-01-07
    let is = "is" **-> skipStringCI "'s" <|> skipStringCI "'re" <|> (__ >>. (keyword ["is"; "was"; "are"; "were"] |>> ignore))
    let says = "says" **-> keyword [ "say"; "says"; "said" ] |>> ignore

    let properNoun = "properNoun" **-> isNotKeyword (many1Chars2 (anyOf uppercaseLetters) (anyOf (letters)))

    let properVariable =
        "properVariable" **->
            sepBy1 properNoun (skipMany1 (pchar ' '))
            |>> fun names -> names |> List.map toLower |> String.concat " " |> Variable

    let commonPrefix =
        "commonPrefix" **->
            keyword commonPrefixList .>> __

    let commonVariable = 
        "commonVariable" **->
            commonPrefix  .>>. isNotKeyword word
            |>> fun (prefix, name') -> Variable (toLower prefix + " " + toLower name')

    let simpleVariable =
        "simpleVariable" **->
            isNotKeyword word
            |>> (toLower >> Variable)
    
    // let pronoun = "pronoun" **-> ((keyword pronounList) .>> followedBy (is <|> says <|> __' <|> EOL <|> eof) >>% Pronoun)
    let pronoun =
        "pronoun" **->
            (keyword pronounList)
            >>% Pronoun

    let variable =
        "variable" **->
            choice [
                pronoun
                commonVariable
                properVariable
                simpleVariable
            ]

    // todo: implement indexing -- bgeiger, 2023-01-08
    let assignable = "assignable" **-> variable

    let (poeticDigit : Parser<_, unit>) = "poeticDigit" **-> many1Chars (anyOf (letters + "-'")) |>> fun s -> String.length (s.Replace("'", "")) % 10
    let poeticDigits =
        "poeticDigits" **->
            many (anyOf poeticDigitSeparators) >>. notEmpty (sepBy' poeticDigit (skipMany (anyOf poeticDigitSeparators)))
            |>> (fun ds -> ds |> List.map (sprintf "%d") |> String.concat "")

    let poeticNumber =
        "poeticNumber" **->
            poeticDigits .>>. (opt (pchar '.' .>>. opt poeticDigits))
            |>> fun (whole, frac') ->
                let frac = 
                    match frac' with
                    | Some (_, Some frac) -> sprintf ".%s" frac
                    | Some (_, None) -> ".0"
                    | None -> ".0"
                (whole + frac) |> float |> NumericValue

    let poeticEmptyString = "poeticEmptyString" **-> keyword ["empty"; "silent"; "silence"] >>% StringValue ""

    let simpleExpression =
        "simpleExpression" **->
            ([
                stringLiteral
                trueConstant
                falseConstant
                numericLiteral
                attempt variable
                poeticEmptyString
            ] |> List.map attempt |> choice)

    let expressionListSeparator =
        "expressionListSeparator" **->
            ([
                skipChar ',' >>. __' >>. skipStringCI "and" >>. __
                skipChar '&'
                skipChar ','
                skipStringCI "'n'"
            ] |> choice) >>. __' |>> ignore

    let expressionList = "expressionList" **-> sepBy' expression expressionListSeparator

    let add = "add" **-> keyword ["+"; "plus"; "with"] .>> __ >>% Add
    let subtract = "subtract" **-> keyword ["-"; "minus"; "without"] .>> __ >>% Subtract
    let multiply = "multiply" **-> keyword ["*"; "times"; "of"] .>> __ >>% Multiply
    let divide = "divide" **-> keyword ["/"; "over"; "between"] .>> __ >>% Divide

    let arithmeticTerm =
        "arithmeticTerm" **->
            simpleExpression .>>. many (attempt (__ >>. (multiply <|> divide) .>> __' .>>. simpleExpression))
            |>> fun (cur, rest) ->
                rest |> List.fold (fun result (curOp, curExpr) -> BinaryOperation (result, curOp, curExpr)) cur

    let arithmetic =
        "arithmetic" **->
            arithmeticTerm .>>. attempt (many (__ >>. (attempt add <|> subtract) .>> __' .>>. arithmeticTerm))
            |>> fun (cur, rest) ->
                rest |> List.fold (fun result (curOp, curExpr) -> BinaryOperation (result, curOp, curExpr)) cur

    let compoundableOperator = [ add; subtract; multiply; divide ] |> List.map attempt |> choice

    let expressionParsers =
        [
            arithmetic
            simpleExpression
        ]

    do expressionRef.Value <- "expression" **-> (expressionParsers |> List.map attempt |> choice)

    let poeticNumericAssignment =
        "poeticNumericAssignment" **->
            assignable .>> is .>> __ .>>. poeticNumber
            |>> fun (variable, value) -> Assignment (variable, value)

    let poeticStringAssignment =
        "poeticStringAssignment" **->
            assignable .>> __ .>> says .>> pchar ' ' .>>. restOfLine false
            |>> fun (variable, value) -> Assignment (variable, StringValue value)

    let compoundAssignment =
        "compoundAssignment" **->
            pstringCI "let" >>. __ >>. assignable .>> __ .>> pstringCI "be" .>> __ .>>. compoundableOperator .>>. expression
            |>> fun ((target, operator), value) -> Assignment (target, BinaryOperation (target, operator, value))

    let directAssignment =
        "directAssignment" **->
            pstringCI "let" >>. __ >>. assignable .>> __ .>> pstringCI "be" .>> __ .>>. expression
            |>> fun (target, value) -> Assignment (target, value)

    let output = "output" **-> (keyword ["say"; "shout"; "whisper"; "scream"]) >>. __ >>. expression |>> Output
    let (nullStatement : Parser<_, unit>) = "nullStatement" **-> preturn Null

    let statementParsers =
        [
            output
            poeticNumericAssignment
            poeticStringAssignment
            directAssignment
            compoundAssignment
            nullStatement
        ]

    do statementRef.Value <- "statement" **-> (statementParsers |> List.map attempt |> choice)
    let line = "line" **-> __' >>. statement .>> (opt noise)
    let program = "program" **-> sepBy line EOL .>> eof

let parse source = runParserOnString Parsers.program () "" source |> function
    | Success (result, _, _) -> Result.Ok result
    | Failure (errorAsString, _, _) -> Result.Error errorAsString

let parseTest source = runParserOnString Parsers.arithmetic () "" source |> function
    | Success (result, _, _) -> Result.Ok result
    | Failure (errorAsString, _, _) -> Result.Error errorAsString
