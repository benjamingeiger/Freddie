namespace Interpreter

open FParsec

module Parser =

    let pronounList = [
        "he"
        "her"
        "him"
        "hir"
        "it"
        "she"
        "them"
        "they"
        "ve"
        "ver"
        "xe"
        "xem"
        "ze"
        "zie"
        "zir"
    ]

    let commonPrefixList = [
        "a"
        "an"
        "my"
        "our"
        "the"
        "your"
    ]

    let literalList = [
        "definitely"
        "empty"
        "false"
        "false"
        "gone"
        "lies"
        "maybe"
        "mysterious"
        "no"
        "nobody"
        "nothing"
        "nowhere"
        "null"
        "ok"
        "silence"
        "silent"
        "true"
        "wrong"
        "yes"
    ]

    let assignmentList = [
        "are"
        "be"
        "in"
        "into"
        "is"
        "let"
        "put"
        "said"
        "say"
        "says"
        "was"
        "were"
    ]

    let operationList = [
        "and"
        "and"
        "around"
        "at"
        "between"
        "build"
        "burn"
        "cast"
        "cut"
        "down"
        "down"
        "into"
        "join"
        "knock"
        "like"
        "minus"
        "nor"
        "not"
        "of"
        "or"
        "over"
        "plus"
        "pop"
        "push"
        "rock"
        "roll"
        "round"
        "shatter"
        "split"
        "times"
        "turn"
        "unite"
        "up"
        "up"
        "with"
        "with"
        "without"
    ]

    let comparisonList = [
        "ain't"
        "aint"
        "aren't"
        "arent"
        "as"
        "big"
        "bigger"
        "great"
        "greater"
        "high"
        "higher"
        "is"
        "isn't"
        "isnt"
        "less"
        "little"
        "low"
        "lower"
        "not"
        "small"
        "smaller"
        "strong"
        "stronger"
        "than"
        "wasn't"
        "wasnt"
        "weak"
        "weaker"
        "weren't"
        "werent"
    ]

    let ioList = [
        "listen"
        "say"
        "scream"
        "shout"
        "to"
        "whisper"
    ]

    let controlFlowList = [
        "break"
        "break"
        "continue"
        "down"
        "else"
        "if"
        "it"
        "take"
        "take"
        "the"
        "to"
        "top"
        "until"
        "while"
    ]

    let functionList = [
        "back"
        "give"
        "return"
        "send"
        "takes"
        "taking"
        "wants"
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


    type Token =
        | Null
        | Break
        | Continue
        | Variable of string
        | Pronoun
        | Function of Token * Token list * Token list

    let toLower (s: string) = if isNull s then s else s.ToLowerInvariant ()

    let isNotKeyword (p : Parser<_, unit>) : Parser<_, unit> =
        fun (stream : CharStream<unit>) ->
            let state = stream.State
            let reply = p stream
            if reply.Status <> Ok || List.contains (toLower reply.Result) keywordList then
                reply
            else
                stream.BacktrackTo state
                Reply(Error, messageError "keyword values are not valid here")


    let uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĸĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"
    let lowercaseLetters = "abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþāăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷÿźżžŉß"
    let letters = uppercaseLetters + lowercaseLetters


    let statement, statementRef = createParserForwardedToRef<Token, unit>()

    let comment = (skipChar '(') >>. (skipCharsTillString ")" true System.Int32.MaxValue)
    let whitespace = many1Chars (anyOf " \t") |>> ignore
    let __ = many (whitespace <|> comment) |>> ignore

    let noiseChars = ";.?!&."
    let noise = skipMany ((__ |>> ignore) <|> skipAnyOf noiseChars)

    let EOL = noise >>. skipNewline
    let EOF = noise >>. eof

    let break_ = pstringCI "break" .>> skipRestOfLine false >>% Break
    let continue_ = (pstringCI "continue" .>> skipRestOfLine false) <|> pstringCI "take it to the top" >>% Continue

    let is = skipStringCI "'s" <|> skipStringCI "'re" <|> (__ >>. (["is"; "was"; "are"; "were"] |> List.map skipStringCI |> choice))

    let name = many1Chars (anyOf letters)
    let simpleVariable = name |>> (toLower >> Variable)
    let commonPrefix = (commonPrefixList |> List.map pstringCI) |> choice
    let commonVariable = commonPrefix .>> __ .>>. name |>> fun (prefix, name) -> Variable (toLower prefix + " " + toLower name)
    let properNoun = isNotKeyword (many1Chars2 (anyOf uppercaseLetters) (anyOf letters))
    let properVariable = sepBy1 properNoun (pchar ' ') |>> fun names -> names |> List.map toLower |> String.concat " " |> Variable
    let pronoun = ((pronounList |> List.map pstringCI) |> choice) .>> notFollowedBy (is <|> __ <|> EOL <|> EOF) >>% Pronoun

    let expressionListSeparator = __ >>. (skipStringCI ", and" .>> (notEmpty __) <|> (["&"; ","; "'n'"] |> List.map skipStringCI |> choice .>> __))
    let variableListSeparator = expressionListSeparator <|> (__ >>. skipStringCI "and" .>> __)

    let variable = choice [simpleVariable; commonVariable; properVariable; pronoun]
    let variableList = sepBy1 variable variableListSeparator

    let block = sepBy1 statement (EOL >>. __)

    let (takes : Parser<unit, unit>) = ["takes"; "wants"] |> List.map skipStringCI |> choice
    let function_ = variable .>> takes .>>. variableList .>> EOL .>>. block .>> EOL |>> (fun ((name, args), body) -> Function (name, args, body))


    do statementRef := __ >>. choice [break_ ; continue_; function_ ]
    let line = __ >>. statement .>> (EOF <|> (skipMany1 EOL))
    let program = many line

    // and statement = __ >>. (break_ <|> continue_)

    let parse = runParserOnString program () ""
