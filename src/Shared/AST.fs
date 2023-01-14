namespace Freddie.Shared

module AST =

    type Identifier = Identifier of string

    and Operator =
        | Add
        | Subtract
        | Multiply
        | Divide

    and Expression =
        | StringValue of string
        | NumericValue of double
        | BooleanValue of bool
        | NullValue
        | MysteriousValue
        | Variable of string
        | Pronoun
        | BinaryOperation of Expression * Operator * Expression

    and Statement =
        | Null
        | Output of Expression
        | Assignment of Expression * Expression

    and Block = Block of Statement list

