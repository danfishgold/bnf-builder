module Main exposing (..)

import Html exposing (Html, program, div, h1, textarea, button, text)
import Html.Attributes exposing (value, rows, cols)
import Html.Events exposing (onInput, onClick)
import Parse exposing (..)
import Model exposing (..)
import Generate exposing (..)


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
    { content = basic, generation = NotGenerated }


basic : String
basic =
    """nonzero ::= 1|2|3|4|5|6|7|8|9
digit ::= 0|1|2|3|4|5|6|7|8|9
two-digit ::= <nonzero><digit>

consonant ::= b|c|d|f|g|k|l|m|n|p|r|s|t|v
vowel ::= a|o|i|u|e
pair ::= <consonant><vowel>
name ::= <pair><pair><consonant>

hello ::= hi | hello | hey

intro ::= <hello>, my name is <name> and I'm <two-digit> years old.
"""


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
    div []
        [ h1 [] [ text "BNF Builder" ]
        , div []
            [ textarea
                [ cols 80
                , rows 25
                , onInput EditContent
                , value model.content
                ]
                []
            ]
        , case parse model.content of
            Ok grammar ->
                div []
                    [ -- div [] [ text <| toString <| grammar ]
                      -- div [] [ render grammar ]
                      div []
                        [ definitionNames grammar
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
                    ]

            Err error ->
                text error
        ]


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
