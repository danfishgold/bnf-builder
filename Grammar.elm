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
        , optionRecalls
        , render
        )

import Html exposing (Html, div, text)
import Dict exposing (Dict)
import Set exposing (Set)


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


fromDefinitionList : List Definition -> Result String Grammar
fromDefinitionList defs =
    let
        dict =
            defs
                |> List.map (\{ name, options } -> ( name, options ))
                |> Dict.fromList

        names =
            List.map .name defs
    in
        case firstError dict names of
            Nothing ->
                Ok <| Grammar ( dict, names )

            Just error ->
                Err error


firstError : Dict String (List Option) -> List String -> Maybe String
firstError dict names =
    case List.head <| Set.toList <| duplicatesInList names of
        Just duplicateDef ->
            Just <| "Duplicate definition <" ++ duplicateDef ++ ">"

        Nothing ->
            case List.head (missingDefinitions dict names) of
                Just missingDef ->
                    Just <| "Missing definition <" ++ missingDef ++ ">"

                Nothing ->
                    let
                        recursives =
                            recursiveDefinitions dict

                        recursivesList =
                            recursives |> Set.map (\name -> "<" ++ name ++ ">")
                    in
                        if Set.isEmpty recursives then
                            Nothing
                        else
                            Just <|
                                "Recursive definitions: "
                                    ++ (recursivesList
                                            |> Set.toList
                                            |> String.join ", "
                                       )


missingDefinitions : Dict String (List Option) -> List String -> List String
missingDefinitions dict names =
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
                |> Set.fromList

        defNames =
            Set.fromList names
    in
        Set.diff recallNames defNames
            |> Set.toList


duplicatesInList : List comparable -> Set comparable
duplicatesInList xs =
    let
        duplicatesHelper xs alreadyDuplicate =
            case xs of
                [] ->
                    Set.fromList alreadyDuplicate

                first :: rest ->
                    if List.member first rest then
                        duplicatesHelper rest (first :: alreadyDuplicate)
                    else
                        duplicatesHelper rest alreadyDuplicate
    in
        duplicatesHelper xs []


recursiveDefinitions : Dict String (List Option) -> Set String
recursiveDefinitions dict =
    let
        areOptionsSafe safeDefinitions opts =
            opts
                |> List.any
                    (optionRecalls
                        >> List.all (flip Set.member safeDefinitions)
                    )

        ( stationaryDefinitions, remainingDefinitions ) =
            dict
                |> Dict.partition (always <| areOptionsSafe Set.empty)
                |> Tuple.mapFirst (Dict.keys >> Set.fromList)

        helper : Set String -> Dict String (List Option) -> Set String
        helper alreadySafe remaining =
            let
                ( newSafe, newRemaining ) =
                    remaining |> Dict.partition (always <| areOptionsSafe alreadySafe)
            in
                if remaining == newRemaining then
                    Set.fromList <| Dict.keys remaining
                else
                    helper
                        (Set.union alreadySafe (Set.fromList <| Dict.keys newSafe))
                        newRemaining
    in
        helper stationaryDefinitions remainingDefinitions


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


optionRecalls : Option -> List String
optionRecalls parts =
    List.filterMap
        (\part ->
            case part of
                Recall defName _ ->
                    Just defName

                _ ->
                    Nothing
        )
        parts


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
