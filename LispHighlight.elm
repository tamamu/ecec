
module LispHighlight exposing (highlight)

import Html exposing (..)
import Html.Attributes exposing (..)
import Parser exposing (Parser, run, map, (|=), (|.), oneOf, succeed, fail, float, symbol, keyword, ignore, keep, repeat, zeroOrMore, oneOrMore)
import Parser.LanguageKit exposing (whitespace, LineComment(..), MultiComment(..))

type Token =
    Space
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

exceptDoubleQuote : Char -> Bool
exceptDoubleQuote c =
  not (String.contains (String.fromChar c) "\"")

parseString : Parser Token
parseString =
  succeed String
        |. symbol "\""
        |=  keep zeroOrMore exceptDoubleQuote
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
        , parseNumber
        , parseString
        , parseKeyword
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
      span [ style [ ("color", "blue") ] ]
      [ text "(" ]
    RightParen ->
      span [ style [ ("color", "blue") ] ]
      [ text ")" ]
    Number num ->
      span [ style [ ("color", "orange") ] ]
      [ text <| toString num ]
    String content ->
      span [ style [ ("color", "green") ] ]
      [ text ("\"" ++ content ++ "\"") ]
    Keyword name ->
      span [ style [ ("color", "yellow") ] ]
      [ text (":" ++ name) ]
    Symbol name ->
      span [ style [ ("color", "black") ] ]
      [ text name ]

highlight : String -> Html msg
highlight src =
  let
    result = run tokenizeSequence src
  in
  case result of
    Ok tokens ->
      span []
      (List.map tokenToHtml tokens)
    Err _ ->
      text "Error"

