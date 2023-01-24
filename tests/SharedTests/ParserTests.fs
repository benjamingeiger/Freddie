module Freddie.Tests.SharedTests

open NUnit.Framework

open Freddie.Shared.Parser
open Freddie.Shared.AST

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``outputting a string literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output (StringValue "herp derp")
        ]

    let source =
        [
            "shout \"herp derp\""
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``outputting a boolean literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output (BooleanValue true)
            Output (BooleanValue false)
        ]

    let source =
        [
            "whisper ok"
            "say lies"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``outputting an empty string works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output (StringValue "")
        ]

    let source =
        [
            "scream silence"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``outputting a numeric literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output (NumericValue 0.123)
            Output (NumericValue 0.456)
            Output (NumericValue 789)
        ]

    let source =
        [
            "shout 0.123"
            "shout .456"
            "shout 789"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``outputting a null literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output NullValue
            Output NullValue
            Output NullValue
            Output NullValue
            Output NullValue
        ]

    let source =
        [
            "shout null"
            "shout nothing"
            "shout nowhere"
            "shout nobody"
            "shout gone"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``assigning a null literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Variable "my life", NullValue)
            Assignment (Variable "my money", NullValue)
            Assignment (Variable "my love", NullValue)
            Assignment (Variable "my friends", NullValue)
            Assignment (Variable "my time", NullValue)
        ]

    let source =
        [
            "my life is null"
            "my money's nothing"
            "my love was nowhere"
            "my friends are nobody"
            "my time is gone"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``outputting a mysterious literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output MysteriousValue
        ]

    let source =
        [
            "shout mysterious"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``assigning a mysterious literal works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Variable "johnny", MysteriousValue)
        ]

    let source =
        [
            "johnny was mysterious"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``outputting a pronoun works`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output Pronoun
            Output Pronoun
            Output Pronoun
        ]

    let source =
        [
            "whisper it"
            "scream them"
            "shout xem"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser handles comments and blank lines`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Null
            Output (StringValue "but not this")
            Null
            Null
            Null
            Output (StringValue "yet another")
        ]

    let source =
        [
            "(ignore this)"
            "(and this) shout \"but not this\""
            ""
            ""
            ""
            "shout \"yet another\""
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser can read variables (common, simple, and proper)`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output (Variable "my name")
            Output (Variable "everything")
            Output (Variable "jumpin jack flash")
        ]

    let source =
        [
            "whisper my name"
            "scream everything"
            "say Jumpin Jack Flash"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser can assign poetic constants`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Variable "my heart", BooleanValue true)
            Assignment (Variable "your soul", BooleanValue false)
        ]

    let source =
        [
            "my heart is true"
            "your soul is lies"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

// TODO: Figure out how to handle tolerance of floating point numbers in NUnit/FsUnit. -- bgeiger, 2023-01-08
[<Test>]
let ``parser can assign poetic numerals`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Variable "my dreams", NumericValue 3.1415926535)
        ]

    let source =
        [
            "My dreams were ice. A life unfulfilled; wakin' everybody up, taking booze and pills"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)


[<Test>]
let ``parser can assign poetic string literals`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Pronoun, StringValue "we'd never make it")
        ]

    let source =
        [
            "They said we'd never make it"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser can perform compound assignment`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Pronoun, BinaryOperation (Pronoun, Subtract, Variable "your love"))
        ]

    let source =
        [
            "Let them be without your love"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser can perform direct assignment`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (Variable "my body", Variable "your flame")
        ]

    let source =
        [
            "Let my body be your flame"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser can handle multiplication and division`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Output (BinaryOperation (Variable "your good", Multiply, Variable "my pain"))
            Output (BinaryOperation (Variable "your time", Divide, Variable "loves"))
        ]

    let source =
        [
            "shout your good times (with) my pain"
            "whisper your time between loves"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)

[<Test>]
let ``parser can handle addition and subtraction with precedence`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (
                Variable "your life",
                BinaryOperation (
                    BinaryOperation (
                        Variable "pain",
                        Add,
                        BinaryOperation (
                            Variable "joy",
                            Divide,
                            Variable "lovers")),
                    Subtract,
                    Variable "fear"))
        ]

    let source =
        [
            "Let your life be pain with joy between lovers without fear"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)


[<Test>]
let ``can parse multiplication with list on the right`` () =
    let expected : Result<_, string> =
        Result.Ok [
            Assignment (
                Variable "the wolf",
                NumericValue 63236)
            Assignment (
                Variable "fear",
                NumericValue 346)
            Assignment (
                Variable "fury",
                NumericValue 355)
            Assignment (
                Variable "hate",
                NumericValue 345)
            Assignment (
                Variable "the wolf",
                BinaryOperation (
                    BinaryOperation (
                        BinaryOperation (
                            Variable "the wolf",
                            Subtract,
                            Variable "fear"),
                        Subtract,
                        Variable "fury"),
                    Subtract,
                    Variable "hate"))
            Output (Variable "the wolf")
        ]

    let source =
        [
            "The wolf is hungry, out on the street (initialise the_wolf = 63236)"
            "Fear is the mind killer (fear = 346)"
            "Fury is the demon child (fury = 355)"
            "Hate is the only truth (hate = 345)"
            "Let the wolf be without fear, fury, and hate (the_wolf = the_wolf - 346 - 355 - 345) "
            "Shout the wolf (output 62190)"
        ] |> String.concat "\n"
    let actual = parse source

    Assert.AreEqual(expected, actual)
