module ShaderParser exposing (..)

import Char
import Parser exposing ((|.), (|=), Parser, map)
import Set


{-
      [identity a = a]


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
     oneOrMore parseValue
     ")"

-}


type alias VariableName =
    String


type Value
    = ValueConstant Constant
    | ValueVariable VariableName
    | ValueExpression (List Assignment) (List Value)


type Constant
    = ConstantString String
    | ConstantInt Int
    | ConstantFloat Float


type alias Assignment =
    { name : String
    , args : List String
    , maybeTypeAnnotation : Maybe (List String)
    , value : Value
    }



--


parse =
    assignment



--


variableName : Parser String
variableName =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '/'
        , reserved = Set.empty
        }


assignment : Parser Assignment
assignment =
    Parser.succeed Assignment
        |. Parser.symbol "["
        |. Parser.spaces
        |= variableName
        |. Parser.spaces
        |= zeroOrMore variableName
        |. Parser.spaces
        |= maybe typeAnnotation
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= value
        |. Parser.spaces
        |. Parser.symbol "]"


typeAnnotation : Parser (List String)
typeAnnotation =
    Parser.succeed identity
        |. Parser.symbol "{"
        |. Parser.spaces
        |= oneOrMore variableName
        |. Parser.spaces
        |. Parser.symbol "}"


value : Parser Value
value =
    Parser.oneOf
        [ Parser.map ValueVariable variableName
        , Parser.lazy (\_ -> parensExpression)
        ]


parensExpression : Parser Value
parensExpression =
    Parser.succeed ValueExpression
        |. Parser.symbol "("
        |. Parser.spaces
        |= zeroOrMore assignment
        |. Parser.spaces
        |= oneOrMore value
        |. Parser.spaces
        |. Parser.symbol ")"



-----------------


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    Parser.oneOf
        [ Parser.map Just parser
        , Parser.succeed Nothing
        ]


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    listHelp parser []


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    parser |> Parser.andThen (\n -> listHelp parser [ n ])


listHelp : Parser a -> List a -> Parser (List a)
listHelp parser listSoFar =
    Parser.oneOf
        [ next parser |> Parser.andThen (\a -> listHelp parser (a :: listSoFar))
        , Parser.succeed (List.reverse listSoFar)
        ]


next : Parser a -> Parser a
next parser =
    Parser.backtrackable <|
        Parser.succeed identity
            |. Parser.spaces
            |= parser



{-


   --------

   map2 : (a -> b -> c) -> Parser ( a, b ) -> Parser c
   map2 f =
       Parser.map (\( a, b ) -> f a b)



   ---




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
-}
