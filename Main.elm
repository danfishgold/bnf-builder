module Main exposing (..)

import Html exposing (Html, program, div, textarea, button, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import Parse exposing (..)
import Model exposing (..)
import Generate exposing (..)
import Parser exposing (run)


type alias Model =
    { content : String, generation : Generation }


type Generation
    = NotGenerated
    | Success String
    | Problem String


type Msg
    = EditContent String
    | RequestGeneration String
    | Generated (Result String String)


model : Model
model =
    { content = "a | b", generation = NotGenerated }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditContent content ->
            ( { model | content = content }, Cmd.none )

        RequestGeneration defName ->
            case parse model.content of
                Ok grammar ->
                    ( model, generate Generated defName grammar )

                Err _ ->
                    ( model, Cmd.none )

        Generated (Ok string) ->
            ( { model | generation = Success string }, Cmd.none )

        Generated (Err error) ->
            ( { model | generation = Problem error }, Cmd.none )


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
            , case ( errs, grammar ) of
                ( [], Ok grm ) ->
                    div []
                        [ definitionNames grm
                            |> List.map
                                (\name ->
                                    button [ onClick <| RequestGeneration name ]
                                        [ text name ]
                                )
                            |> div []
                        , case model.generation of
                            Success generated ->
                                text generated

                            _ ->
                                text ""
                        ]

                _ ->
                    text ""
            ]


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
