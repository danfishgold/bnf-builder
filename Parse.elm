module Parse exposing (..)

import Model exposing (..)
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Char
import Set
import Dict


parse : String -> Result String Grammar
parse content =
    run (definitions |. end) content
        |> Result.mapError (.problem >> toString)
        |> Result.map grammarFromDefs
        |> Result.andThen
            (\grammar ->
                case errors grammar of
                    [] ->
                        Ok grammar

                    firstError :: _ ->
                        Err firstError
            )


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


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


whitespaces : Parser ()
whitespaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


newLines : Parser ()
newLines =
    ignore zeroOrMore (\c -> c == '\n')
