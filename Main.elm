module Main exposing (..)

import Editor
import Html exposing (Html)


type alias Model =
    { editor : Editor.Model
    }


model : Model
model =
    { editor = Editor.model
    }


type Msg
    = Editor Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Editor msg ->
            let
                ( editor, cmd ) =
                    Editor.update msg model.editor
            in
            { model | editor = editor } ! [ Cmd.map Editor cmd ]


view : Model -> Html Msg
view model =
    Html.map Editor (Editor.render model.editor)


main =
    Html.program
        { view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map Editor (Editor.subscriptions model.editor)
                    ]
        , init = ( model, Editor.init )
        }
