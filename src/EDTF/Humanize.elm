module EDTF.Humanize exposing (humanize)

import EDTF exposing (DateAnnotated, EDTF(..), Season(..))


humanize : EDTF -> String
humanize date =
    case date of
        Single dd ->
            humanizeSingle dd

        Interval mdd1 mdd2 ->
            humanizeInterval mdd1 mdd2

        List dateList ->
            List.map humanize dateList
                |> String.join "; "


humanizeSingle : DateAnnotated -> String
humanizeSingle annotated =
    ""


humanizeInterval : Maybe DateAnnotated -> Maybe DateAnnotated -> String
humanizeInterval maybeStart maybeEnd =
    ""


toStringSeason : Season -> String
toStringSeason season =
    case season of
        Spring ->
            "Spring"

        Summer ->
            "Summer"

        Autumn ->
            "Autumn"

        Winter ->
            "Winter"


toStringMonth : Int -> String
toStringMonth mm =
    ""
