module Model exposing (..)

import Html exposing (Html, div, text)
import Dict exposing (Dict)
import Set


type alias Definition =
    { name : String, options : List Option }


type Grammar
    = Grammar ( Dict String (List Option), List String )


type alias Option =
    List OptionPart


type OptionPart
    = Str String
    | Special Char
    | Recall String String


grammarFromDefs : List Definition -> Grammar
grammarFromDefs defs =
    let
        pairs =
            defs
                |> List.map (\{ name, options } -> ( name, options ))

        names =
            defs |> List.map .name
    in
        Grammar ( Dict.fromList pairs, names )


defsFromGrammar : Grammar -> List Definition
defsFromGrammar (Grammar ( dict, defNames )) =
    let
        definition name =
            case Dict.get name dict of
                Nothing ->
                    Nothing

                Just options ->
                    Just { name = name, options = options }
    in
        defNames |> List.filterMap definition


definitionNames : Grammar -> List String
definitionNames (Grammar ( _, defNames )) =
    defNames


definitionDict : Grammar -> Dict String (List Option)
definitionDict (Grammar ( dict, _ )) =
    dict


errors : Grammar -> List String
errors (Grammar ( dict, defNames )) =
    let
        getRecallDefName optionPart =
            case optionPart of
                Recall defName _ ->
                    Just defName

                _ ->
                    Nothing

        recallNames =
            dict
                |> Dict.values
                |> List.concat
                |> List.concat
                |> List.filterMap getRecallDefName

        missingDefinitions =
            Set.diff (Set.fromList recallNames) (Set.fromList defNames)
    in
        missingDefinitions
            |> Set.toList
            |> List.map (\defName -> "Can't find <" ++ defName ++ ">")


render : Grammar -> Html msg
render =
    defsFromGrammar >> renderDefinitions


renderDefinitions : List Definition -> Html msg
renderDefinitions definitions =
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

        Special char ->
            "\\" ++ String.fromChar char

        Recall defName options ->
            "<" ++ defName ++ "|" ++ options ++ ">"
