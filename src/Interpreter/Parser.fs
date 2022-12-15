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

    let rec comment = (skipChar '(') >>. (charsTillString ")" true System.Int32.MaxValue)
    and whitespace = many1Chars (anyOf " \t")
    and blank = many1 (whitespace <|> comment)
    and line = optional blank >>. pstring "foo"  .>> skipRestOfLine true
    and program = many line

    let parse = runParserOnString program () ""
