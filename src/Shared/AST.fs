namespace Freddie.Shared

module AST =

    type Identifier = Identifier of string

    and Expression =
        | StringLiteral of string
        | NumericLiteral of double
        | BooleanLiteral of bool
        | Variable of string
        | Pronoun

    and Statement =
        | Null
        | Output of Expression

    and Block = Block of Statement list

