module Editor
    exposing
        ( Model
        , Msg
        , init
        , model
        , render
        , subscriptions
        , update
        )

import Char exposing (fromCode)
import Dom exposing (..)
import Html exposing (..)
import Html.Attributes exposing (defaultValue, id, style, value)
import Html.Events as Events exposing (on, onBlur, onClick, onFocus, onWithOptions)
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
    , rangeFrom : CaretPosition
    , tabSize : Int
    , tabStyle : TabStyle
    }


type TabStyle
    = Spaces
    | Tab


type alias CaretPosition =
    { col : Int, row : Int }


model : String -> Model
model id =
    { id = id
    , isFocused = False
    , content = [ "" ]
    , lastKey = ""
    , caretPos = { col = 0, row = 0 }
    , isSelected = False
    , rangeFrom = { col = 0, row = 0 }
    , tabSize = 4
    , tabStyle = Tab
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


rangeContent : List String -> CaretPosition -> CaretPosition -> List String
rangeContent src from to =
    let
        fromSmaller =
            from.row < to.row || from.col < to.col
    in
    if fromSmaller then
        let
            rowStart =
                from.row

            rowEnd =
                to.row

            colStart =
                from.col

            colEnd =
                to.col

            rows =
                List.drop rowStart src |> List.take rowEnd

            last =
                List.length rows - 1
        in
        List.map
            (\t ->
                let
                    ( i, s ) =
                        t
                in
                if i == 0 then
                    String.dropLeft colStart s
                else if i == last then
                    String.left colEnd s
                else
                    s
            )
        <|
            List.indexedMap (,) rows
    else
        let
            rowStart =
                to.row

            rowEnd =
                from.row

            colStart =
                to.col

            colEnd =
                from.col

            rows =
                List.drop rowStart src |> List.take rowEnd

            last =
                List.length rows - 1
        in
        List.map
            (\t ->
                let
                    ( i, s ) =
                        t
                in
                if i == 0 then
                    String.dropLeft colStart s
                else if i == last then
                    String.left colEnd s
                else
                    s
            )
        <|
            List.indexedMap (,) rows


desc : CaretPosition -> CaretPosition -> ( CaretPosition, CaretPosition )
desc a b =
    if a.row == b.row then
        if a.col <= b.col then
            ( a, b )
        else
            ( b, a )
    else if a.row < b.row then
        ( a, b )
    else
        ( b, a )


replaceRange : List String -> CaretPosition -> CaretPosition -> String -> List String
replaceRange src from_ to_ data =
    let
        pair =
            desc from_ to_

        from =
            Tuple.first pair

        to =
            Tuple.second pair

        lineLength =
            to.row - from.row

        lines =
            String.lines data

        currentLine =
            Maybe.withDefault "" <| getAt from.row src

        lastLine =
            Maybe.withDefault "" <| getAt to.row src

        top =
            if lineLength > 1 then
                Maybe.withDefault "" <| List.head lines
            else
                (Maybe.withDefault "" <| List.head lines) ++ String.dropLeft (to.col + 1) lastLine

        inner =
            if lineLength > 2 then
                List.take (lineLength - 2) <| List.drop 1 lines
            else
                []

        bottom =
            if lineLength > 1 then
                [ Maybe.withDefault "" (List.head <| List.drop (lineLength - 1) lines) ++ String.right to.col lastLine ]
            else
                []
    in
    List.concat <| List.take from.row src :: [ String.left from.col currentLine ++ top ] :: inner :: bottom :: [ List.drop (to.row + 1) src ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        caretPos =
            model.caretPos
    in
    case msg of
        FocusOn id ->
            if model.id == id then
                model ! [ Task.attempt FocusResult (Dom.focus id) ]
            else
                model ! []

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
                        if model.rangeFrom.row < model.caretPos.row || model.rangeFrom.col < model.caretPos.col then
                            model.rangeFrom.col
                        else
                            model.caretPos.col
                in
                { model
                    | content = replaceRange model.content model.rangeFrom caretPos data
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
                currentLine =
                    toCodePoints <| Maybe.withDefault "" <| List.Extra.getAt model.caretPos.row model.content

                caretPos =
                    moveCaretLeft { row = model.caretPos.row, col = min model.caretPos.col (List.length currentLine) }

                rangeLength =
                    abs (caretPos.col - model.rangeFrom.col)
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
                    abs (caretPos.col - model.rangeFrom.col)
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
                    update
                        (Insert
                            (case model.tabStyle of
                                Spaces ->
                                    String.repeat model.tabSize " "

                                Tab ->
                                    "\t"
                            )
                        )
                        model

                "Escape" ->
                    model ! []

                "Alphanumeric" ->
                    model ! []

                "NonConvert" ->
                    model ! []

                "ArrowLeft" ->
                    let
                        rangeLength =
                            abs (model.rangeFrom.col - model.caretPos.col)

                        rangeFrom =
                            model.rangeFrom
                    in
                    if shift then
                        if model.isSelected then
                            update MoveLeft model
                        else
                            update MoveLeft
                                { model
                                    | isSelected = True
                                    , rangeFrom = { rangeFrom | col = model.caretPos.col }
                                }
                    else if model.isSelected then
                        update MoveLeft
                            { model
                                | isSelected = False
                                , caretPos = { caretPos | col = min model.rangeFrom.col model.caretPos.col + 1 }
                            }
                    else
                        update MoveLeft model

                "ArrowRight" ->
                    let
                        rangeLength =
                            abs (model.rangeFrom.col - model.caretPos.col)

                        rangeFrom =
                            model.rangeFrom
                    in
                    if shift then
                        if model.isSelected then
                            update MoveRight model
                        else
                            update MoveRight
                                { model
                                    | isSelected = True
                                    , rangeFrom = { rangeFrom | col = model.caretPos.col }
                                }
                    else if model.isSelected then
                        update MoveRight
                            { model
                                | isSelected = False
                                , caretPos = { caretPos | col = max model.rangeFrom.col model.caretPos.col - 1 }
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
render : Model -> Html Msg
render model =
    view model


view : Model -> Html Msg
view model =
    let
        rangeDisplay =
            if model.isSelected then
                ( "display", "block" )
            else
                ( "display", "none" )

        caretDisplay =
            if model.isFocused then
                ( "visibility", "visible" )
            else
                ( "visibility", "hidden" )

        currentLine =
            Maybe.withDefault "" <| List.Extra.getAt model.caretPos.row model.content

        codePoints =
            toCodePoints currentLine

        rangeLeft =
            min model.rangeFrom.col model.caretPos.col

        rangeLength =
            abs (model.rangeFrom.col - model.caretPos.col)

        caretText =
            fromCodePoints <| List.take model.caretPos.col codePoints

        rangeLeftText =
            fromCodePoints <| List.take rangeLeft codePoints

        rangeInnerText =
            fromCodePoints <| List.take rangeLength <| List.drop rangeLeft codePoints

        tabSize =
            ( "-moz-tab-size", toString <| model.tabSize )
    in
    div
        [ style
            [ ( "font-family", "monospace" )
            ]
        ]
        [ stylesheet
        , textarea
            [ onPaste Insert
            , onKeyDown KeyDown
            , onCompositionEnd Insert
            , onFocus Focused
            , onBlur Blured
            , defaultValue ""
            , value ""
            , id model.id
            , style
                [ ( "position", "absolute" )
                , ( "width", "0px" )
                , ( "height", "0px" )
                , ( "outline", "0" )
                , ( "margin", "0" )
                , ( "padding", "0" )
                , ( "border", "0px" )
                , ( "z-index", "0" )
                , ( "top", toString model.caretPos.row ++ "em" )
                , ( "left", toString model.caretPos.col ++ "em" )
                , tabSize
                ]
            ]
            []
        , div [ style [ ( "display", "flex" ) ] ]
            [ div
                [ style
                    [ ( "background-color", "gray" )
                    , ( "border", "1px solid black" )
                    , ( "border-right", "0" )
                    , ( "padding-right", "10px" )
                    ]
                ]
              <|
                List.map
                    (\i ->
                        span
                            [ style
                                [ ( "display", "block" )
                                , ( "height", "1em" )
                                , tabSize
                                ]
                            ]
                            [ text <| toString i ]
                    )
                <|
                    List.range 1 <|
                        List.length model.content
            , div
                [ style
                    [ ( "display", "block" )
                    , ( "border", "1px solid black" )
                    , ( "width", "30em" )
                    , ( "height", (toString <| List.length model.content) ++ "em" )
                    , ( "cursor", "text" )
                    ]
                , onClick (FocusOn model.id)
                ]
                [ span
                    [ style
                        [ ( "position", "absolute" )
                        , rangeDisplay
                        ]
                    ]
                    [ span
                        [ style
                            [ ( "visibility", "hidden" )
                            , ( "white-space", "pre" )
                            , tabSize
                            ]
                        ]
                        [ text rangeLeftText ]
                    , span
                        [ style
                            [ ( "opacity", "0.5" )
                            , ( "background-color", "gray" )
                            , ( "white-space", "pre" )
                            , tabSize
                            ]
                        ]
                        [ text rangeInnerText ]
                    ]
                , span [ style [ ( "position", "absolute" ) ] ]
                    [ span
                        [ style
                            [ ( "visibility", "hidden" )
                            , ( "white-space", "pre" )
                            , tabSize
                            ]
                        ]
                        [ text caretText ]
                    , span
                        [ style
                            [ ( "display", "inline-block" )
                            , caretDisplay
                            , ( "animation", "blink .5s alternate infinite ease-in" )
                            , ( "transform", "translateX(-4px) translateY(" ++ toString model.caretPos.row ++ "em)" )
                            ]
                        ]
                        [ text "|" ]
                    ]
                , div [] <|
                    List.map
                        (\s ->
                            div
                                [ style
                                    [ ( "display", "block" )
                                    , ( "width", "30em" )
                                    , ( "height", "1em" )
                                    , tabSize
                                    ]
                                ]
                                [ highlight s ]
                        )
                        model.content
                ]
            ]
        , div [ style [ ( "display", "none" ) ] ]
            [ text <| toString model.lastKey
            , text ","
            , text <| toString model.caretPos
            ]
        ]


stylesheet : Html a
stylesheet =
    Html.node "style"
        []
        [ Html.text """
    @keyframes blink {
      from {
        opacity: 1;
      }
      to {
        opacity: 0;
      }
    }
""" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Cmd m
init =
    Cmd.none
