module Main exposing (..)

import Html exposing (Html, program, div, h1, span, textarea, button, text)
import Html.Attributes exposing (value, rows, style, dir)
import Html.Events exposing (onInput, onClick)
import Parse exposing (parse)
import Grammar
import Generate exposing (generate)
import Assets
import Examples


type alias Model =
    { content : String
    , textDirection : TextDirection
    , generation : Generation
    }


type TextDirection
    = Ltr
    | Rtl


dirAttr : TextDirection -> Html.Attribute msg
dirAttr direction =
    case direction of
        Rtl ->
            dir "rtl"

        Ltr ->
            dir "ltr"


type Generation
    = NotGenerated
    | Success String
    | Problem String


type Msg
    = EditContent String
    | SetDirection TextDirection
    | SetContentAndDirection String TextDirection
    | RequestGeneration String
    | Generated (Result String String)


model : Model
model =
    { content = Examples.intro
    , textDirection = Ltr
    , generation = NotGenerated
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditContent content ->
            ( { model | content = content }, Cmd.none )

        SetDirection dir ->
            ( { model | textDirection = dir }, Cmd.none )

        SetContentAndDirection content dir ->
            ( { model | content = content, textDirection = dir }, Cmd.none )

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
            [ div []
                [ span [ onClick <| SetDirection Ltr ] [ Assets.alignLeft "3em" ]
                , span [ onClick <| SetDirection Rtl ] [ Assets.alignRight "3em" ]
                ]
            , div []
                [ button [ onClick <| SetContentAndDirection Examples.intro Ltr ] [ text "Intro" ]
                , button [ onClick <| SetContentAndDirection Examples.rumors Rtl ] [ text "Rumors" ]
                , button [ onClick <| SetContentAndDirection "" model.textDirection ] [ text "Clear" ]
                ]
            , textarea
                [ style [ ( "width", "80%" ) ]
                , rows 51
                , onInput EditContent
                , value model.content
                , dirAttr model.textDirection
                ]
                []
            ]
        , case parse model.content of
            Ok grammar ->
                div []
                    [ -- div [] [ text <| toString <| grammar ]
                      -- div [] [ render grammar ]
                      div []
                        [ Grammar.definitionNames grammar
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
