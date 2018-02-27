
import Task
import Dom exposing (..)
import Css exposing (..)
import Css.Colors exposing (..)
import Css.Foreign exposing (global, selector)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id, css, defaultValue, value)
import Html.Styled.Events as Events exposing (on, onWithOptions, onClick, onFocus, onBlur)
import Char exposing (fromCode)
import Json.Decode as Json
import String.Extra exposing (toCodePoints, fromCodePoints)

{-| Event property decoder
-}
compositionData : Json.Decoder String
compositionData =
  Json.at [ "data" ] Json.string

decodedKey : Json.Decoder String
decodedKey =
  Json.at [ "key" ] Json.string

shiftKey : Json.Decoder Bool
shiftKey =
  Json.at [ "shiftKey" ] Json.bool


{-| Event handler
-}
onCompositionEnd : (String -> msg) -> Attribute msg
onCompositionEnd handler =
  let
    eventOptions =
      { stopPropagation = True, preventDefault = True }
  in
  onWithOptions "compositionend" eventOptions (Json.map handler compositionData)

onPaste : (String -> msg) -> Attribute msg
onPaste handler =
  let
    eventOptions =
      { stopPropagation = True, preventDefault = True }
  in
  onWithOptions "paste" eventOptions (Json.map handler Events.targetValue)

onInput : (String -> msg) -> Attribute msg
onInput handler =
  let
    eventOptions =
      { stopPropagation = True, preventDefault = True }
  in
  onWithOptions "input" eventOptions (Json.map handler Events.targetValue)

onChange : (String -> msg) -> Attribute msg
onChange handler =
  let
    eventOptions =
      { stopPropagation = True, preventDefault = True }
  in
  onWithOptions "change" eventOptions (Json.map handler Events.targetValue)

onKeyUp : (String -> msg) -> Attribute msg
onKeyUp handler =
  let
    eventOptions =
      { stopPropagation = True, preventDefault = True }
  in
  onWithOptions "keyup" eventOptions (Json.map handler decodedKey)

onKeyDown : (String -> Bool -> msg) -> Attribute msg
onKeyDown handler =
  let
    eventOptions =
      { stopPropagation = True, preventDefault = True }
  in
  onWithOptions "keydown" eventOptions (Json.map2 handler decodedKey shiftKey)

{-| Model
-}
type alias Model =
  { id : String
  , isFocused : Bool
  , content : String
  , lastKey : String
  , caretColumn : Int
  , caretRow : Int
  , isSelected : Bool
  , rangeFrom : Int
  }

model : Model
model =
  { id = "editor"
  , isFocused = False
  , content = ""
  , lastKey = ""
  , caretColumn = 0
  , caretRow = 0
  , isSelected = False
  , rangeFrom = 0
  }

{-| Update
-}
type Msg
  = FocusOn String
  | FocusResult (Result Dom.Error ())
  | Focused
  | Blured
  | Insert String
  | Backspace
  | SetText String
  | KeyDown String Bool
  | KeyUp String
  | MoveLeft
  | MoveRight

moveCaretLeft : Int -> Int
moveCaretLeft pos =
  max 0 (pos - 1)

moveCaretRight : Int -> Int -> Int
moveCaretRight pos max =
  min max (pos + 1)

charsLength : String -> Int
charsLength string =
  List.length <| toCodePoints string

stringReplace : String -> Int -> Int -> String -> String
stringReplace insert start end string =
  let
      chars = toCodePoints string
  in
  fromCodePoints (List.take start chars) ++ insert ++ fromCodePoints (List.drop end chars)

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
  case msg of
    FocusOn id ->
      model ! [ Task.attempt FocusResult (Dom.focus id) ]
    FocusResult result ->
      case result of
        Err (Dom.NotFound id) ->
          model ! []
        Ok () ->
          model ! []
    Focused ->
      { model | isFocused = True } ! []
    Blured ->
      { model | isFocused = False } ! []
    Insert data ->
      if model.isSelected then
        let
            rangeLeft = min model.rangeFrom model.caretColumn
            rangeRight = max model.rangeFrom model.caretColumn
        in
        { model | content = stringReplace data rangeLeft rangeRight model.content
        , caretColumn = rangeLeft
        , isSelected = False
        } ! []
      else
        { model | content = stringReplace data model.caretColumn model.caretColumn model.content
        , caretColumn = model.caretColumn + charsLength data
        } ! []
    Backspace ->
      { model | content = stringReplace "" (model.caretColumn - 1) model.caretColumn model.content
      , caretColumn = moveCaretLeft model.caretColumn} ! []
    SetText text ->
      { model | content = String.concat [model.content , text]
      , caretColumn = model.caretColumn + (String.length text - String.length model.content)} ! []
    MoveLeft ->
      let
          caretColumn = moveCaretLeft model.caretColumn
          rangeLength = abs (caretColumn - model.rangeFrom)
      in
      { model | caretColumn = caretColumn
      , isSelected = model.isSelected && rangeLength > 0
      } ! []
    MoveRight ->
      let
          caretColumn = moveCaretRight model.caretColumn (charsLength model.content)
          rangeLength = abs (caretColumn - model.rangeFrom)
      in
      { model | caretColumn = caretColumn
      , isSelected = model.isSelected && rangeLength > 0
      } ! []
    KeyDown key shift ->
      case key of
        "Backspace" ->
          if model.isSelected then
            update (Insert "") { model | lastKey = key }
          else
            update Backspace { model | lastKey = key }
        "Shift" -> model ! []
        "Enter" -> model ! []
        "Control" -> model ! []
        "Alt" -> model ! []
        "Tab" -> model ! []
        "Escape" -> model ! []
        "Alphanumeric" -> model ! []
        "NonConvert" -> model ! []
        "ArrowLeft" ->
          let
              rangeLength = abs (model.rangeFrom - model.caretColumn)
          in
          if shift then
            if model.isSelected then
              update MoveLeft model
          else
              update MoveLeft { model | isSelected = True
                              , rangeFrom = model.caretColumn
                              }
          else
            if model.isSelected then
              update MoveLeft { model | isSelected = False
                              , caretColumn = (min model.rangeFrom model.caretColumn) + 1
                              }
            else
              update MoveLeft model
        "ArrowRight" ->
          let
              rangeLength = abs (model.rangeFrom - model.caretColumn)
          in
          if shift then
            if model.isSelected then
              update MoveRight model
          else
              update MoveRight { model | isSelected = True
                               , rangeFrom = model.caretColumn
                               }
          else
            if model.isSelected then
              update MoveRight { model | isSelected = False
                               , caretColumn = (max model.rangeFrom model.caretColumn) - 1
                               }
            else
              update MoveRight model
        _ ->
          if String.length key == 1 then
            update (Insert key) model
          else
            { model | lastKey = key } ! []
    KeyUp key ->
      model ! []

{-| View
-}
view : Model -> Html Msg
view model =
  let
      rangeDisplay =
        if model.isSelected then
          display block
        else
          display none
      caretDisplay =
        if model.isFocused then
          visibility visible
        else
          visibility hidden
      codePoints = toCodePoints model.content
      rangeLeft = min model.rangeFrom model.caretColumn
      rangeLength = abs (model.rangeFrom - model.caretColumn)
      caretText = fromCodePoints <| List.take model.caretColumn codePoints
      rangeLeftText = fromCodePoints <| List.take rangeLeft codePoints
      rangeInnerText = fromCodePoints <| List.take rangeLength <| List.drop rangeLeft codePoints
  in
  div [ css [ margin (px 32) ] ]
    [ textarea [ onPaste Insert
               , onKeyDown KeyDown
               , onCompositionEnd Insert
               , onFocus Focused
               , onBlur Blured
               , defaultValue ""
               , value ""
               , id model.id
               , css [ position absolute
                     , width (px 0)
                     , height (px 0)
                     , outline zero
                     , margin zero
                     , padding zero
                     , border (px 0)
                     , zIndex (int 0)
                     ]
               ] []
    , div [ css [ display block
                , border3 (px 1) solid black
                , width (px 200)
                , height (Css.em 1)
                , cursor text_
                ]
          , onClick (FocusOn model.id ) ]
      [ span [ css [ position absolute
                   , rangeDisplay
                   ]
             ]
        [ span [ css [ visibility hidden ] ]
          [ text rangeLeftText ]
        , span [ css [ opacity (Css.num 0.5)
                     , backgroundColor gray
                     ]
               ]
          [ text rangeInnerText ]
        ]
      , span [ css [ position absolute ] ]
        [ span [ css [ visibility hidden ] ]
          [ text caretText ]
        , span [ css [ caretDisplay
                     , property "animation" "blink .5s alternate infinite ease-in" ] ]
          [ text "|" ]
        ]
      , text model.content
      ]
    , text <| toString model.lastKey
    , text ","
    , text <| toString model.caretColumn
    ]



main =
  Html.program
      { view = view >> toUnstyled
      , update = update
      , subscriptions = \_ -> Sub.none
      , init = ( model, Cmd.none )
      }

