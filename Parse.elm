module Parse exposing (..)

import Model exposing (..)
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Char
import Set


parse : String -> Result Error (List Definition)
parse content =
    run (definitions |. end) content


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
