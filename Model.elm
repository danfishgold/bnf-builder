module Model exposing (..)

import Html exposing (Html, div, text)


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
