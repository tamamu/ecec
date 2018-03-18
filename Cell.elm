module Cell
    exposing
        ( Model
        , Msg
        , init
        , model
        , render
        , subscriptions
        , update
        )

import Editor
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type alias Model =
    { id : Int
    , cellType : CellType
    }


type CellType
    = Code CodeModel
    | Note
    | Raw


type alias CodeModel =
    { executionCount : ExecutionCount
    , editor : Editor.Model
    , output : Maybe String
    }


type alias ExecutionCount =
    Maybe Int


type alias ExecutionOutput =
    Maybe String


model : Int -> Model
model id =
    { id = id
    , cellType = Code (codeModel id)
    }


codeModel : Int -> CodeModel
codeModel id =
    { executionCount = Nothing
    , editor = Editor.model (toString id)
    , output = Nothing
    }


type Msg
    = Editor Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.cellType of
        Code cm ->
            case msg of
                Editor msg ->
                    let
                        ( editor, cmd ) =
                            Editor.update msg cm.editor

                        codeModel : CodeModel
                        codeModel =
                            { cm | editor = editor }
                    in
                    { model | cellType = Code codeModel } ! [ Cmd.map Editor cmd ]

        _ ->
            model ! []


viewExecutionCount : ExecutionCount -> Html Msg
viewExecutionCount model =
    div [ style [ ( "padding-right", "8px" ) ] ]
        [ case model of
            Just count ->
                text <| toString count

            Nothing ->
                text "-"
        ]


viewOutput : ExecutionOutput -> Html Msg
viewOutput model =
    case model of
        Just str ->
            div
                [ style
                    [ ( "border", "1px solid black" )
                    , ( "border-top", "0px" )
                    , ( "height", "1em" )
                    ]
                ]
                [ text str ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    div [ style [ ( "margin", "16px" ) ] ]
        [ case model.cellType of
            Code cm ->
                div [ style [ ( "display", "flex" ) ] ]
                    [ viewExecutionCount cm.executionCount
                    , div []
                        [ Html.map Editor (Editor.render cm.editor)
                        , viewOutput cm.output
                        ]
                    ]

            Note ->
                text "This is note cell"

            Raw ->
                text "This is raw cell"
        ]


render : Model -> Html Msg
render model =
    view model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.cellType of
        Code cm ->
            Sub.batch
                [ Sub.map Editor (Editor.subscriptions cm.editor) ]

        Note ->
            Sub.none

        Raw ->
            Sub.none


init : ( Model, Cmd m )
init =
    ( model 0, Editor.init )


main =
    Html.program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }
