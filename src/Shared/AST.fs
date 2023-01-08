namespace Freddie.Shared

module AST =

    type Identifier = Identifier of string

    and Expression =
        | StringValue of string
        | NumericValue of double
        | BooleanValue of bool
        | Variable of string
        | Pronoun

    and Statement =
        | Null
        | Output of Expression
        | Assignment of Expression * Expression

    and Block = Block of Statement list

