module LispHighlight exposing (highlight)

import Html exposing (..)
import Html.Attributes exposing (..)
import Parser exposing ((|.), (|=), Count(..), Parser, end, fail, float, ignore, keep, keyword, map, oneOf, oneOrMore, repeat, run, source, succeed, symbol, zeroOrMore)
import Parser.LanguageKit exposing (LineComment(..), MultiComment(..), whitespace)


type Token
    = Space
    | LeftParen
    | RightParen
    | Symbol String
    | Number Float
    | String String
    | Keyword String


symbolChar : Char -> Bool
symbolChar c =
    String.contains (String.fromChar c) "abcdefghijklmnopqrstuvwxyz1234567890!#$%&@^|/:[]{}?-=_*+."


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
            |. ignore oneOrMore (\c -> c == ' ')
        , succeed LeftParen
            |. symbol "("
        , succeed RightParen
            |. symbol ")"
        , parseString
        , parseKeyword
        , parseNumber
        , parseSymbol
        ]


tokenizeSequence : Parser (List Token)
tokenizeSequence =
    repeat zeroOrMore tokenize


tokenToHtml : Token -> Html msg
tokenToHtml token =
    case token of
        Space ->
            text " "

        LeftParen ->
            span [ style [ ( "color", "blue" ) ] ]
                [ text "(" ]

        RightParen ->
            span [ style [ ( "color", "blue" ) ] ]
                [ text ")" ]

        Number num ->
            span [ style [ ( "color", "orange" ) ] ]
                [ text <| toString num ]

        String content ->
            span [ style [ ( "color", "green" ) ] ]
                [ text ("\"" ++ content ++ "\"") ]

        Keyword name ->
            span [ style [ ( "color", "yellow" ) ] ]
                [ text (":" ++ name) ]

        Symbol name ->
            span [ style [ ( "color", "black" ) ] ]
                [ text name ]


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
            text src
