module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, textarea, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Set
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
        definitions =
            parse model.content
    in
        div []
            [ div [] [ textarea [ onInput EditContent, value model.content ] [] ]
            , div [] [ text <| toString <| run options model.content ]
            , div [] [ text <| toString <| definitions ]
            , div [] [ Result.withDefault (text "") <| Result.map render <| definitions ]
            , case (definitions |> Result.map errors |> Result.withDefault []) of
                [] ->
                    text ""

                errs ->
                    errs |> List.map (text >> List.singleton >> div []) |> div []
            ]


errors : List Definition -> List String
errors definitions =
    let
        defNames =
            definitions
                |> List.map .name
                |> Set.fromList

        getRecallDefName optionPart =
            case optionPart of
                Recall defName _ ->
                    Just defName

                _ ->
                    Nothing

        recallNames =
            definitions
                |> List.concatMap .options
                |> List.concatMap identity
                |> List.filterMap getRecallDefName
                |> Set.fromList

        missingDefinitions =
            Set.diff recallNames defNames
    in
        missingDefinitions |> Set.toList |> List.map (\defName -> "Can't find <" ++ defName ++ ">")


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , update = update
        , view = view
        }
