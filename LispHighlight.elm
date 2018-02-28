module LispHighlight exposing (highlight)

import Html exposing (..)
import Html.Attributes exposing (..)
import Parser exposing ((|.), (|=), Count(..), Parser, end, fail, float, ignore, keep, keyword, map, oneOf, oneOrMore, repeat, run, source, succeed, symbol, zeroOrMore)
import Parser.LanguageKit exposing (LineComment(..), MultiComment(..), whitespace)


type Token
    = Space String
    | LeftParen
    | RightParen
    | Symbol String
    | Number Float
    | String String
    | Keyword String
    | Other String


symbolChar : Char -> Bool
symbolChar c =
    String.contains (String.fromChar c) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!#$%&@^|/:[]{}?-=_*+."


exceptDelim : Char -> Bool
exceptDelim c =
    not (String.contains (String.fromChar c) "(), \t\n':;")


parseExceptDoubleQuote : Parser String
parseExceptDoubleQuote =
    keep (Exactly 1) (\c -> not (String.contains (String.fromChar c) "\""))


parseEscapedChar : Parser String
parseEscapedChar =
    Parser.source <|
        ignore (Exactly 1) (\c -> c == '\\')
            |. ignore (Exactly 1) (\c -> True)


parseStringChar : Parser String
parseStringChar =
    oneOf
        [ parseEscapedChar, parseExceptDoubleQuote ]


parseString : Parser Token
parseString =
    succeed String
        |. symbol "\""
        |= Parser.map String.concat (repeat zeroOrMore parseStringChar)
        |. symbol "\""


parseKeyword : Parser Token
parseKeyword =
    succeed Keyword
        |. symbol ":"
        |= keep oneOrMore exceptDelim


parseSymbol : Parser Token
parseSymbol =
    succeed Symbol
        |= keep oneOrMore symbolChar


parseNumber : Parser Token
parseNumber =
    succeed Number
        |= float


tokenize : Parser Token
tokenize =
    oneOf
        [ succeed Space
            --|. (whitespace { allowTabs = True , lineComment = LineComment ";" , multiComment = NestableComment "#|" "|#" })
            |= keep oneOrMore (\c -> c == ' ')
        , succeed LeftParen
            |. symbol "("
        , succeed RightParen
            |. symbol ")"
        , parseString
        , parseKeyword
        , parseNumber
        , parseSymbol
        , succeed Other
            |= keep oneOrMore (\c -> True)
        ]


tokenizeSequence : Parser (List Token)
tokenizeSequence =
    repeat zeroOrMore tokenize


tokenToHtml : Token -> Html msg
tokenToHtml token =
    let
        display =
            ( "display", "inline" )

        pre =
            ( "white-space", "pre" )
    in
    case token of
        Space content ->
            span
                [ style
                    [ display
                    , pre
                    ]
                ]
                [ text content ]

        LeftParen ->
            span
                [ style
                    [ display
                    , pre
                    , ( "color", "blue" )
                    ]
                ]
                [ text "(" ]

        RightParen ->
            span
                [ style
                    [ display
                    , pre
                    , ( "color", "blue" )
                    ]
                ]
                [ text ")" ]

        Number num ->
            span
                [ style
                    [ display
                    , pre
                    , ( "color", "orange" )
                    ]
                ]
                [ text <| toString num ]

        String content ->
            span
                [ style
                    [ display
                    , pre
                    , ( "color", "green" )
                    ]
                ]
                [ text ("\"" ++ content ++ "\"") ]

        Keyword name ->
            span
                [ style
                    [ display
                    , pre
                    , ( "color", "cyan" )
                    ]
                ]
                [ text (":" ++ name) ]

        Symbol name ->
            span
                [ style
                    [ display
                    , pre
                    , ( "color", "black" )
                    ]
                ]
                [ text name ]

        Other content ->
            span
                [ style
                    [ display
                    , pre
                    , ( "background-color", "red" )
                    ]
                ]
                [ text content ]


highlight : String -> Html msg
highlight src =
    let
        result =
            run tokenizeSequence src
    in
    case result of
        Ok tokens ->
            span []
                (List.map tokenToHtml tokens)

        Err _ ->
            span
                [ style
                    [ ( "display", "inline" )
                    , ( "white-space", "pre" )
                    ]
                ]
                [ text src ]
