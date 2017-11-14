module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, textarea, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Char
import Set


type alias Model =
    { content : String }


type Msg
    = EditContent String


model : Model
model =
    { content = "" }


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


parse : String -> Result Error (List Definition)
parse content =
    run (definitions |. end) content


type alias Definition =
    { name : String, options : List Option }


type alias Option =
    List OptionPart


type OptionPart
    = Str String
    | SpecialSpace
    | SpecialPipe
    | SpecialOpenBrackets
    | SpecialCloseBrackets
    | SpecialBackslash
    | Recall String String


definition : Parser Definition
definition =
    succeed Definition
        |= defName
        |. spaces
        |. symbol "::="
        |. spaces
        |= options


definitions : Parser (List Definition)
definitions =
    repeat zeroOrMore
        (succeed identity
            |. newLines
            |= definition
            |. newLines
        )


options : Parser (List Option)
options =
    sequence
        { start = ""
        , separator = "|"
        , end = ""
        , spaces = spaces
        , item = option
        , trailing = Forbidden
        }


option : Parser Option
option =
    succeed identity
        |. spaces
        |= repeat oneOrMore optionPart
        |. spaces


optionPart : Parser OptionPart
optionPart =
    oneOf
        [ succeed Recall
            |. symbol "<"
            |= defName
            |= oneOf
                [ succeed identity
                    |. symbol "|"
                    |= defName
                , succeed ""
                ]
            |. symbol ">"
        , variable isNormalChar isNormalChar Set.empty |> map Str
        , succeed SpecialSpace |. symbol "\\ "
        , succeed SpecialPipe |. symbol "\\|"
        , succeed SpecialOpenBrackets |. symbol "\\<"
        , succeed SpecialCloseBrackets |. symbol "\\>"
        , succeed SpecialBackslash |. symbol "\\\\"
        ]


isNormalChar : Char -> Bool
isNormalChar c =
    c /= '<' && c /= '>' && c /= '|' && c /= '\\' && c /= '\n'


defName : Parser String
defName =
    variable isVarChar isVarChar Set.empty


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || (char == '_')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


newLines : Parser ()
newLines =
    ignore zeroOrMore (\c -> c == '\n')


render : List Definition -> Html msg
render definitions =
    definitions
        |> List.map
            (renderDefinition
                >> text
                >> List.singleton
                >> div []
            )
        |> div []


renderDefinition : Definition -> String
renderDefinition { name, options } =
    name ++ " ::= " ++ renderOptions options


renderOptions : List Option -> String
renderOptions options =
    options |> List.map renderOption |> String.join "|"


renderOption : List OptionPart -> String
renderOption optionParts =
    optionParts |> List.map renderOptionPart |> String.join ""


renderOptionPart : OptionPart -> String
renderOptionPart optionPart =
    case optionPart of
        Str str ->
            str

        SpecialSpace ->
            "\\ "

        SpecialPipe ->
            "\\|"

        SpecialOpenBrackets ->
            "\\<"

        SpecialCloseBrackets ->
            "\\>"

        SpecialBackslash ->
            "\\\\"

        Recall defName options ->
            "<" ++ defName ++ "|" ++ options ++ ">"


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
