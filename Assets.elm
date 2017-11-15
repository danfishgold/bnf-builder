module Assets exposing (alignRight, alignLeft)

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (..)
import Svg.Attributes exposing (..)


alignLeft : String -> Html msg
alignLeft sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title
            []
            [ text "Icons 200" ]
        , Svg.path [ d "M14.0207,46.0207h24a2,2,0,0,1,0,4h-24a2,2,0,0,1,0-4Zm36-4h-36a2,2,0,0,1,0-4h36a2,2,0,0,1,0,4Zm-36-12h34a2,2,0,0,1,0,4h-34a2,2,0,0,1,0-4Zm0-8h28a2,2,0,1,1,0,4h-28a2,2,0,0,1,0-4Zm36-4h-36a2,2,0,1,1,0-4h36a2,2,0,1,1,0,4Z", attribute "fill-rule" "evenodd" ]
            []
        ]


alignRight : String -> Html msg
alignRight sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title
            []
            [ text "Icons 200" ]
        , Svg.path [ d "M50.0208,50.0208h-24a2,2,0,0,1,0-4h24a2,2,0,0,1,0,4Zm0-8h-36a2,2,0,0,1,0-4h36a2,2,0,0,1,0,4Zm0-8h-34a2,2,0,1,1,0-4h34a2,2,0,1,1,0,4Zm0-8h-28a2,2,0,1,1,0-4h28a2,2,0,1,1,0,4Zm0-8h-36a2,2,0,1,1,0-4h36a2,2,0,1,1,0,4Z", attribute "fill-rule" "evenodd" ]
            []
        ]
