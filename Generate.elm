module Generate exposing (generate)

import Grammar exposing (Grammar, Option, OptionPart(..))
import Random exposing (Generator)
import Random.Extra exposing (constant, sample)


type alias GenRes =
    Result String String


generate : (GenRes -> msg) -> String -> Grammar -> Cmd msg
generate toMsg defName grammar =
    Random.generate toMsg (definitionGenerator grammar defName)


definitionGenerator : Grammar -> String -> Generator GenRes
definitionGenerator grammar defName =
    Grammar.getOptions defName grammar
        |> Result.map sample
        |> Result.map (Random.andThen (generateMaybeOption grammar defName))
        |> flattenResult


generateMaybeOption : Grammar -> String -> Maybe Option -> Generator GenRes
generateMaybeOption grammar defName maybeOption =
    case maybeOption of
        Nothing ->
            constant <| Err <| "<" ++ defName ++ "> has no options"

        Just parts ->
            parts
                |> List.map (generatePart grammar)
                |> Random.Extra.combine
                |> Random.map okOrFirstErr
                |> Random.map (Result.map (String.join ""))


okOrFirstErr : List (Result x a) -> Result x (List a)
okOrFirstErr results =
    case results of
        [] ->
            Ok []

        (Ok okHead) :: tail ->
            okOrFirstErr tail |> Result.map (\okTail -> okHead :: okTail)

        (Err errHead) :: _ ->
            Err errHead


flattenResult : Result x (Generator (Result x a)) -> Generator (Result x a)
flattenResult res =
    case res of
        Err error ->
            constant (Err error)

        Ok generator ->
            generator


generatePart : Grammar -> OptionPart -> Generator GenRes
generatePart grammar part =
    case part of
        Str string ->
            constant <| Ok string

        Special char ->
            constant <| Ok <| String.fromChar char

        Recall defName _ ->
            definitionGenerator grammar defName
