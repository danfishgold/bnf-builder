module Parse exposing (parse)

import Grammar exposing (Grammar, Definition, Option, OptionPart(..))
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Char
import Set exposing (Set)
import Dict exposing (Dict)


parse : String -> Result String Grammar
parse content =
    run (definitions |. end) content
        |> Result.mapError (toString)
        |> Result.map Grammar.fromDefinitionList
        |> Result.andThen
            (\grammar ->
                grammar
                    |> firstError
                    |> Maybe.map Err
                    |> Maybe.withDefault (Ok grammar)
            )


firstError : Grammar -> Maybe String
firstError grammar =
    case List.head (missingDefinitions grammar) of
        Just missingDef ->
            Just <| "Missing definition <" ++ missingDef ++ ">"

        Nothing ->
            let
                recursives =
                    recursiveDefinitions grammar

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


missingDefinitions : Grammar -> List String
missingDefinitions grammar =
    let
        getRecallDefName optionPart =
            case optionPart of
                Recall defName _ ->
                    Just defName

                _ ->
                    Nothing

        recallNames =
            Grammar.definitionDict grammar
                |> Dict.values
                |> List.concat
                |> List.concat
                |> List.filterMap getRecallDefName
                |> Set.fromList

        defNames =
            Grammar.definitionNames grammar
                |> Set.fromList
    in
        Set.diff recallNames defNames
            |> Set.toList


recursiveDefinitions grammar =
    let
        areOptionsSafe safeDefinitions opts =
            opts
                |> List.any
                    (Grammar.optionRecalls
                        >> List.all (flip Set.member safeDefinitions)
                    )

        dict =
            Grammar.definitionDict grammar

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


definition : Parser Definition
definition =
    succeed Definition
        |= defName
        |. spaces
        |. symbol "::="
        |. whitespaces
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
        , spaces = whitespaces
        , item = option
        , trailing = Forbidden
        }


option : Parser Option
option =
    succeed trimOption
        |. spaces
        |= repeat oneOrMore optionPart
        |. spaces


trimOptionLeft : Option -> Option
trimOptionLeft parts =
    case parts of
        (Str string) :: tail ->
            Str (String.trimLeft string) :: tail

        _ ->
            parts


trimOptionRight : Option -> Option
trimOptionRight parts =
    case List.reverse parts of
        (Str string) :: tail ->
            Str (String.trimRight string) :: tail |> List.reverse

        _ ->
            parts


trimOption : Option -> Option
trimOption =
    trimOptionRight >> trimOptionLeft


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
        , succeed (Special ' ') |. symbol "\\ "
        , succeed (Special '|') |. symbol "\\|"
        , succeed (Special '<') |. symbol "\\<"
        , succeed (Special '>') |. symbol "\\>"
        , succeed (Special '\\') |. symbol "\\\\"
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
        || (char == '-')
        || List.member char hebrewLetters


hebrewLetters : List Char
hebrewLetters =
    [ 'א'
    , 'ב'
    , 'ג'
    , 'ד'
    , 'ה'
    , 'ו'
    , 'ז'
    , 'ח'
    , 'ט'
    , 'י'
    , 'כ'
    , 'ל'
    , 'מ'
    , 'נ'
    , 'ס'
    , 'ע'
    , 'פ'
    , 'צ'
    , 'ק'
    , 'ר'
    , 'ש'
    , 'ת'
    , 'ן'
    , 'ף'
    , 'ך'
    , 'ץ'
    , 'ם'
    ]


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


whitespaces : Parser ()
whitespaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


newLines : Parser ()
newLines =
    ignore zeroOrMore (\c -> c == '\n')
