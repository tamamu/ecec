module Main exposing (..)

import Cell
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type alias Model =
    { cells : List Cell.Model }


model : Model
model =
    { cells =
        [ Cell.model 1
        , Cell.model 2
        , Cell.model 3
        ]
    }


type Msg
    = Cell Int Cell.Msg


updateCell : Int -> Cell.Msg -> Cell.Model -> ( Cell.Model, Cmd Cell.Msg )
updateCell id msg model =
    if model.id == id then
        Cell.update msg model
    else
        model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Cell id msg ->
            let
                ( cells, cmds ) =
                    List.unzip <|
                        List.map (updateCell id msg) model.cells
            in
            { model | cells = cells } ! List.map (\cmd -> Cmd.map (Cell id) cmd) cmds


view : Model -> Html Msg
view model =
    div [] <|
        List.map (\cell -> Html.map (Cell cell.id) (Cell.render cell)) model.cells


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.map
            (\cell ->
                Sub.map (Cell cell.id) (Cell.subscriptions cell)
            )
            model.cells


main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \m -> Sub.none
        , init = ( model, Cmd.none )
        }
