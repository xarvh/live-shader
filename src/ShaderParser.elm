module ShaderParser exposing (..)

import Char
import Parser exposing ((|.), (|=), Parser, map)
import Set


{-


   [addition a b {number number number}
     (+ a b)
   ]


   [someFunction b {number number}
     [s 0]
     [e 20]

     (- e s)
   ]



    (if someCondition
      (hugeIfTrue)

      (
        [ x {number} 3 ]
        [ y -2 ]
        dotProduct x y
      )

    )



parseAssignment
  "["
  oneOrMore parseVariable
  maybe parseType
  parseValue
  "]"


parseType
  "{"
  oneOrMore parseVariable
  "}"


parseValue
  oneOf
    parseConstant
    parseVariable
    parseParens


parseParens
  "("
  oneOrMore parseAssignment
  oneOrMore
    parseValue
  ")"

-}


map2 : (a -> b -> c) -> Parser ( a, b ) -> Parser c
map2 f =
    Parser.map (\( a, b ) -> f a b)



---


type alias VariableName =
    String


type Expression
    = AttributeAccess Expression AttributeName
    | AttributeGetter AttributeName
    | Variable VariableName
    | ConstantInteger Int
    | ConstantFloat Float
    | Parens Expression
    | UnaryOperation Expression
    | FunctionCall Expression (List Expression)
    | BinaryOperation Expression Expression
    | LetIn (List Assignment) Expression


expression : Parser Expression
expression =
    [ Parser.lazy (\_ -> map2 AttributeAccess attributeAccess)
    , Parser.map AttributeGetter attributeGetter
    , Parser.map Variable variable
    , Parser.map ConstantInteger Parser.int
    , Parser.map ConstantFloat Parser.float
    , Parser.lazy (\_ -> map Parens parensExpression)
    ]
        -- TODO This is the anti-patternest of them all, but will stay here
        -- until Parser gets better docs.
        |> List.map Parser.backtrackable
        |> Parser.oneOf


variable : Parser String
variable =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedNames
        }


parensExpression : Parser Expression
parensExpression =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= expression
        |. Parser.spaces
        |. Parser.symbol ")"


attributeAccess : Parser ( Expression, AttributeName )
attributeAccess =
    Parser.succeed Tuple.pair
        |= Parser.oneOf [ parensExpression, map Variable variable ]
        |. Parser.symbol "."
        |= variable


attributeGetter : Parser AttributeName
attributeGetter =
    Parser.succeed identity
        |. Parser.symbol "."
        |= variable


parse : Parser Expression
parse =
    Parser.succeed identity
        |= expression
        |. Parser.end
