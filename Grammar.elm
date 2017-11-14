module Grammar
    exposing
        ( Grammar
        , Definition
        , Option
        , OptionPart(..)
        , fromDefinitionList
        , toDefinitionList
        , definitionNames
        , definitionDict
        , getOptions
        , render
        )

import Html exposing (Html, div, text)
import Dict exposing (Dict)


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


fromDefinitionList : List Definition -> Grammar
fromDefinitionList defs =
    let
        pairs =
            defs
                |> List.map (\{ name, options } -> ( name, options ))

        names =
            defs |> List.map .name
    in
        Grammar ( Dict.fromList pairs, names )


toDefinitionList : Grammar -> List Definition
toDefinitionList (Grammar ( dict, defNames )) =
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


getOptions : String -> Grammar -> Result String (List Option)
getOptions defName (Grammar ( dict, _ )) =
    case Dict.get defName dict of
        Nothing ->
            Err <| "Can't find <" ++ defName ++ ">"

        Just options ->
            Ok options


render : Grammar -> Html msg
render =
    toDefinitionList >> renderDefinitions


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
            -- "<" ++ defName ++ "|" ++ options ++ ">"
            "<" ++ defName ++ ">"
