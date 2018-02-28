module Main exposing (..)

import Char exposing (fromCode)
import Css exposing (..)
import Css.Colors exposing (..)
import Css.Foreign exposing (global, selector)
import Dom exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, defaultValue, id, value)
import Html.Styled.Events as Events exposing (on, onBlur, onClick, onFocus, onWithOptions)
import Json.Decode as Json
import LispHighlight exposing (highlight)
import List.Extra exposing (getAt)
import String.Extra exposing (fromCodePoints, toCodePoints)
import Task


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
    , content : List String
    , lastKey : String
    , caretPos : CaretPosition
    , isSelected : Bool
    , rangeFrom : Int
    }


type alias CaretPosition =
    { col : Int, row : Int }


model : Model
model =
    { id = "editor"
    , isFocused = False
    , content = [ "" ]
    , lastKey = ""
    , caretPos = { col = 0, row = 0 }
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
    | NewLine
    | KeyDown String Bool
    | KeyUp String
    | MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown


moveCaretLeft : CaretPosition -> CaretPosition
moveCaretLeft pos =
    { pos | col = max 0 (pos.col - 1) }


moveCaretRight : CaretPosition -> Int -> CaretPosition
moveCaretRight pos max =
    { pos | col = min max (pos.col + 1) }


moveCaretUp : CaretPosition -> CaretPosition
moveCaretUp pos =
    { pos | row = max 0 (pos.row - 1) }


moveCaretDown : CaretPosition -> Int -> CaretPosition
moveCaretDown pos max =
    { pos | row = min max (pos.row + 1) }


charsLength : String -> Int
charsLength string =
    List.length <| toCodePoints string


stringReplace : String -> Int -> Int -> String -> String
stringReplace insert start end string =
    let
        chars =
            toCodePoints string
    in
    fromCodePoints (List.take start chars) ++ insert ++ fromCodePoints (List.drop end chars)


updateContent : List String -> Int -> (String -> String) -> List String
updateContent src row fun =
    List.map
        (\t ->
            let
                ( i, s ) =
                    t
            in
            if i == row then
                fun s
            else
                s
        )
    <|
        List.indexedMap (,) src


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        caretPos =
            model.caretPos
    in
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
                    rangeLeft =
                        min model.rangeFrom model.caretPos.col

                    rangeRight =
                        max model.rangeFrom model.caretPos.col
                in
                { model
                    | content = updateContent model.content caretPos.row <| stringReplace data rangeLeft rangeRight
                    , caretPos = { caretPos | col = rangeLeft }
                    , isSelected = False
                }
                    ! []
            else
                { model
                    | content = updateContent model.content caretPos.row <| stringReplace data model.caretPos.col model.caretPos.col
                    , caretPos = { caretPos | col = model.caretPos.col + charsLength data }
                }
                    ! []

        Backspace ->
            if model.caretPos.col == 0 && model.caretPos.row > 0 then
                let
                    prevLine =
                        toCodePoints <| Maybe.withDefault "" <| List.Extra.getAt (model.caretPos.row - 1) model.content

                    currentLine =
                        toCodePoints <| Maybe.withDefault "" <| List.Extra.getAt model.caretPos.row model.content

                    right =
                        List.drop model.caretPos.col currentLine

                    line =
                        List.concat [ prevLine, right ]

                    top =
                        List.take (model.caretPos.row - 1) model.content

                    bottom =
                        List.drop (model.caretPos.row + 1) model.content
                in
                { model
                    | content = List.concat [ top, [ fromCodePoints line ], bottom ]
                    , caretPos = { caretPos | col = List.length line, row = model.caretPos.row - 1 }
                }
                    ! []
            else
                { model
                    | content = updateContent model.content caretPos.row <| stringReplace "" (model.caretPos.col - 1) model.caretPos.col
                    , caretPos = moveCaretLeft model.caretPos
                }
                    ! []

        SetText text ->
            { model
                | content = updateContent model.content caretPos.row <| \s -> String.concat [ s, text ]
                , caretPos = { caretPos | col = 0, row = 0 }
            }
                ! []

        NewLine ->
            let
                currentLine =
                    toCodePoints <| Maybe.withDefault "" <| List.Extra.getAt model.caretPos.row model.content

                left : String
                left =
                    fromCodePoints <| List.take model.caretPos.col currentLine

                right : String
                right =
                    fromCodePoints <| List.drop model.caretPos.col currentLine

                top : List String
                top =
                    List.take model.caretPos.row model.content

                bottom : List String
                bottom =
                    List.drop (model.caretPos.row + 1) model.content
            in
            { model
                | content = List.concat [ top, [ left, right ], bottom ]
                , caretPos = { caretPos | col = 0, row = model.caretPos.row + 1 }
            }
                ! []

        MoveLeft ->
            let
                caretPos =
                    moveCaretLeft model.caretPos

                rangeLength =
                    abs (caretPos.col - model.rangeFrom)
            in
            { model
                | caretPos = caretPos
                , isSelected = model.isSelected && rangeLength > 0
            }
                ! []

        MoveRight ->
            let
                caretPos =
                    moveCaretRight model.caretPos (charsLength <| Maybe.withDefault "" <| List.Extra.getAt model.caretPos.row model.content)

                rangeLength =
                    abs (caretPos.col - model.rangeFrom)
            in
            { model
                | caretPos = caretPos
                , isSelected = model.isSelected && rangeLength > 0
            }
                ! []

        MoveUp ->
            let
                caretPos =
                    moveCaretUp model.caretPos
            in
            { model
                | caretPos = caretPos
                , isSelected = model.isSelected
            }
                ! []

        MoveDown ->
            let
                caretPos =
                    moveCaretDown model.caretPos (List.length model.content - 1)
            in
            { model
                | caretPos = caretPos
                , isSelected = model.isSelected
            }
                ! []

        KeyDown key shift ->
            case key of
                "Backspace" ->
                    if model.isSelected then
                        update (Insert "") { model | lastKey = key }
                    else
                        update Backspace { model | lastKey = key }

                "Shift" ->
                    model ! []

                "Enter" ->
                    update NewLine model

                "Control" ->
                    model ! []

                "Alt" ->
                    model ! []

                "Tab" ->
                    model ! []

                "Escape" ->
                    model ! []

                "Alphanumeric" ->
                    model ! []

                "NonConvert" ->
                    model ! []

                "ArrowLeft" ->
                    let
                        rangeLength =
                            abs (model.rangeFrom - model.caretPos.col)
                    in
                    if shift then
                        if model.isSelected then
                            update MoveLeft model
                        else
                            update MoveLeft
                                { model
                                    | isSelected = True
                                    , rangeFrom = model.caretPos.col
                                }
                    else if model.isSelected then
                        update MoveLeft
                            { model
                                | isSelected = False
                                , caretPos = { caretPos | col = min model.rangeFrom model.caretPos.col + 1 }
                            }
                    else
                        update MoveLeft model

                "ArrowRight" ->
                    let
                        rangeLength =
                            abs (model.rangeFrom - model.caretPos.col)
                    in
                    if shift then
                        if model.isSelected then
                            update MoveRight model
                        else
                            update MoveRight
                                { model
                                    | isSelected = True
                                    , rangeFrom = model.caretPos.col
                                }
                    else if model.isSelected then
                        update MoveRight
                            { model
                                | isSelected = False
                                , caretPos = { caretPos | col = max model.rangeFrom model.caretPos.col - 1 }
                            }
                    else
                        update MoveRight model

                "ArrowUp" ->
                    update MoveUp model

                "ArrowDown" ->
                    update MoveDown model

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

        currentLine =
            Maybe.withDefault "" <| List.Extra.getAt model.caretPos.row model.content

        codePoints =
            toCodePoints currentLine

        rangeLeft =
            min model.rangeFrom model.caretPos.col

        rangeLength =
            abs (model.rangeFrom - model.caretPos.col)

        caretText =
            fromCodePoints <| List.take model.caretPos.col codePoints

        rangeLeftText =
            fromCodePoints <| List.take rangeLeft codePoints

        rangeInnerText =
            fromCodePoints <| List.take rangeLength <| List.drop rangeLeft codePoints
    in
    div
        [ css
            [ margin (px 32)
            , fontFamily monospace
            ]
        ]
        [ textarea
            [ onPaste Insert
            , onKeyDown KeyDown
            , onCompositionEnd Insert
            , onFocus Focused
            , onBlur Blured
            , defaultValue ""
            , value ""
            , id model.id
            , css
                [ position absolute
                , width (px 0)
                , height (px 0)
                , outline zero
                , margin zero
                , padding zero
                , border (px 0)
                , zIndex (int 0)
                ]
            ]
            []
        , div [ css [ displayFlex ] ]
            [ div
                [ css
                    [ backgroundColor gray
                    , border3 (px 1) solid black
                    , borderRight (px 0)
                    , paddingRight (px 10)
                    ]
                ]
              <|
                List.map
                    (\i ->
                        span
                            [ css
                                [ display block
                                , height (Css.em 1)
                                ]
                            ]
                            [ text <| toString i ]
                    )
                <|
                    List.range 1 <|
                        List.length model.content
            , div
                [ css
                    [ display block
                    , border3 (px 1) solid black
                    , width (Css.em 30)
                    , height (Css.em <| toFloat <| List.length model.content)
                    , cursor text_
                    ]
                , onClick (FocusOn model.id)
                ]
                [ span
                    [ css
                        [ position absolute
                        , rangeDisplay
                        ]
                    ]
                    [ span
                        [ css
                            [ visibility hidden
                            , whiteSpace Css.pre
                            ]
                        ]
                        [ text rangeLeftText ]
                    , span
                        [ css
                            [ opacity (Css.num 0.5)
                            , backgroundColor gray
                            , whiteSpace Css.pre
                            ]
                        ]
                        [ text rangeInnerText ]
                    ]
                , span [ css [ position absolute ] ]
                    [ span
                        [ css
                            [ visibility hidden
                            , whiteSpace Css.pre
                            ]
                        ]
                        [ text caretText ]
                    , span
                        [ css
                            [ display inlineBlock
                            , caretDisplay
                            , property "animation" "blink .5s alternate infinite ease-in"
                            , transform (translateY (Css.em <| toFloat model.caretPos.row))
                            ]
                        ]
                        [ text "|" ]
                    ]
                , div [] <|
                    List.map
                        (\s ->
                            div
                                [ css
                                    [ display block
                                    , width (Css.em 30)
                                    , height (Css.em 1)
                                    ]
                                ]
                                [ fromUnstyled (highlight s) ]
                        )
                        model.content
                ]
            ]
        , text <| toString model.lastKey
        , text ","
        , text <| toString model.caretPos
        ]


main =
    Html.program
        { view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
