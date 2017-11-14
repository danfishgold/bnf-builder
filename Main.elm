module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, textarea, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Parse exposing (..)
import Model exposing (..)
import Parser exposing (run)


type alias Model =
    { content : String }


type Msg
    = EditContent String


model : Model
model =
    { content = "a | b" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditContent content ->
            { model | content = content }


view : Model -> Html Msg
view model =
    let
        grammar =
            parse model.content

        errs =
            grammar |> Result.map errors |> Result.withDefault []
    in
        div []
            [ div [] [ textarea [ onInput EditContent, value model.content ] [] ]
            , div [] [ text <| toString <| run options model.content ]
            , div [] [ text <| toString <| grammar ]
            , div [] [ Result.withDefault (text "") <| Result.map render <| grammar ]
            , case errs of
                [] ->
                    text ""

                errs ->
                    errs |> List.map (text >> List.singleton >> div []) |> div []

            ]


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , update = update
        , view = view
        }
