module EDTF exposing
    ( EDTF(..), DateAnnotated, DateValue(..), Season(..), Month(..)
    , parse, toString
    )

{-| A small, dependency-free Elm parser for a practical subset of
Extended Date/Time Format (EDTF). It uses `Parser` from the Elm core
library and aims to cover common cases while keeping the API tidy.

Supported (Level 0 + useful Level 1 bits):

  - Calendar dates: `YYYY`, `YYYY-MM`, `YYYY-MM-DD`
  - Expanded years via `Y` prefix: `Y-12034`, `Y19999`
  - Seasons (Level 1): `YYYY-21` (Spring), `YYYY-22` (Summer),
    `YYYY-23` (Autumn), `YYYY-24` (Winter)
  - Uncertainty `?` and approximation `~` suffix markers, alone or combined
    in any order (e.g. `2020?`, `2020~`, `2020?~`, `2020-05~?`)
  - Intervals: `<edtf>/<edtf>` where either side can be a real date or
    open using `..` or the word `open` (non-standard but practical)
  - Lists: comma-separated items, e.g. `2020,2021-05,2021-21`

Deliberately **not** implemented (for now):

  - Sets/choices with braces `{}` / `..` in sets and the full Level 2
    qualifiers
  - Week dates / ordinal dates / times / time zones

@docs EDTF, DateAnnotated, DateValue, Season, Month

@docs parse, toString

-}

import Parser
    exposing
        ( DeadEnd
        , Parser
        , Step(..)
        , andThen
        , chompIf
        , chompWhile
        , end
        , getChompedString
        , keyword
        , loop
        , map
        , oneOf
        , problem
        , run
        , succeed
        , symbol
        )
import Parser.Advanced as P



-- TYPES


{-| High-level AST for EDTF values.
-}
type EDTF
    = Single DateAnnotated
    | Interval (Maybe DateAnnotated) (Maybe DateAnnotated)
    | List (List EDTF)


{-| A date value with uncertainty/approximation flags.
-}
type alias DateAnnotated =
    { date : DateValue
    , uncertain : Bool
    , approximate : Bool
    }


{-| Concrete date variants we parse.
-}
type DateValue
    = YMD Int Month Int
    | YM Int Month
    | Y Int
    | Season Int Season


{-| Seasons as per EDTF numeric codes 21..24.
-}
type Season
    = Spring
    | Summer
    | Autumn
    | Winter


{-| Custom type for months of the year
-}
type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May_
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


{-| Internal type for parsed year components
-}
type alias YearDigits =
    { sign : String
    , digits : String
    }



-- PUBLIC API


{-| Parse a string into an `EDTF` AST.

    parse "2020-05-17"
        == Ok (Single { date = YMD 2020 5 17, uncertain = False, approximate = False })

    parse "2020?~"
        == Ok (Single { date = Y 2020, uncertain = True, approximate = True })

    parse "2020/.."
        == Ok (Interval (Just { date = Y 2020, uncertain = False, approximate = False }) Nothing)

    parse "2020,2021-21"
        == Ok
            (List
                [ Single { date = Y 2020, uncertain = False, approximate = False }
                , Single { date = Season 2021 Spring, uncertain = False, approximate = False }
                ]
            )

-}
parse : String -> Result (List DeadEnd) EDTF
parse input =
    run
        (spaces
            |> andThen (\_ -> pEdtf)
            |> andThen (\v -> end |> map (\_ -> v))
        )
        input


{-| Render an `EDTF` AST back to a string (best-effort, normalized).
-}
toString : EDTF -> String
toString value =
    case value of
        Single d ->
            dateAnnotatedToString d

        Interval a b ->
            maybeAnnotatedToString a ++ "/" ++ maybeAnnotatedToString b

        List xs ->
            xs |> List.map toString |> String.join ","



-- INTERNAL PARSERS


pEdtf : Parser EDTF
pEdtf =
    P.oneOf
        [ P.backtrackable pInterval
        , P.backtrackable pMaskedInterval
        , pListOrSingle
        ]


pListOrSingle : Parser EDTF
pListOrSingle =
    pAtom
        |> andThen
            (\first ->
                oneOf
                    [ comma |> andThen (\_ -> loop [ first ] accumulateItems)
                    , succeed first
                    ]
            )


accumulateItems : List EDTF -> Parser (Step (List EDTF) EDTF)
accumulateItems accRev =
    oneOf
        [ pAtom
            |> andThen
                (\x ->
                    oneOf
                        [ comma |> map (\_ -> Loop (x :: accRev))
                        , succeed (Done (wrapList (x :: accRev)))
                        ]
                )
        , succeed (Done (wrapList accRev))
        ]


wrapList : List EDTF -> EDTF
wrapList items =
    List (List.reverse items)


{-| Parse a single atom: masked interval, normal interval, or single date
-}
pAtom : Parser EDTF
pAtom =
    P.oneOf
        [ P.backtrackable pMaskedInterval
        , P.backtrackable pInterval
        , map Single pDateAnnotated
        ]


comma : Parser ()
comma =
    spaces
        |> andThen (\_ -> symbol ",")
        |> andThen (\_ -> spaces)



-- INTERVAL PARSERS


{-| Support: "/<right>" (unknown start), "<left>/" (unknown end), and "<left>/<right>"
-}
pInterval : Parser EDTF
pInterval =
    oneOf
        [ symbol "/"
            |> andThen (\_ -> pMaybeAnnotated)
            |> map (\b -> Interval Nothing b)
        , pMaybeAnnotated
            |> andThen
                (\a ->
                    symbol "/"
                        |> andThen
                            (\_ ->
                                P.oneOf
                                    [ P.backtrackable pMaybeAnnotated
                                        |> map (\b -> Interval a b)
                                    , succeed (Interval a Nothing)
                                    ]
                            )
                )
        ]


pMaybeAnnotated : Parser (Maybe DateAnnotated)
pMaybeAnnotated =
    oneOf
        [ symbol ".." |> map (\_ -> Nothing)
        , keyword "open" |> map (\_ -> Nothing)
        , pDateAnnotated |> map Just
        ]



-- MASKED INTERVAL PARSER


{-| Parse masked forms with 'X' and convert to an Interval with closed bounds.
Supported shapes:
"199X" -> 1990..1999 (year precision)
"19XX" -> 1900..1999
"-01XX" -> -0199..-0100
"1999-XX" -> 1999-01..1999-12
"1999-01-XX" -> 1999-01-01..1999-01-31
"1999-XX-XX" -> 1999-01-01..1999-12-31
"XXXX-XX-23" -> 0000-01-23..9999-12-23
-}
pMaskedInterval : Parser EDTF
pMaskedInterval =
    parseYearDigits
        |> andThen
            (\yDigits ->
                let
                    yMask =
                        yDigits.sign ++ yDigits.digits

                    hasXYear =
                        String.contains "X" yDigits.digits
                in
                oneOf
                    [ symbol "-"
                        |> andThen (\_ -> parseMaskedMonth hasXYear yMask)
                    , if hasXYear then
                        maskedToInterval yMask Nothing Nothing

                      else
                        problem "Not a masked date"
                    ]
            )


parseMaskedMonth : Bool -> String -> Parser EDTF
parseMaskedMonth hasXYear yMask =
    monthString
        |> andThen
            (\mm ->
                let
                    hasXMonth =
                        mm == "XX"
                in
                oneOf
                    [ symbol "-"
                        |> andThen (\_ -> parseMaskedDay hasXYear hasXMonth yMask mm)
                    , if hasXYear || hasXMonth then
                        maskedToInterval yMask (Just mm) Nothing

                      else
                        problem "Not a masked date"
                    ]
            )


parseMaskedDay : Bool -> Bool -> String -> String -> Parser EDTF
parseMaskedDay hasXYear hasXMonth yMask mm =
    dayString
        |> andThen
            (\dd ->
                let
                    hasXDay =
                        dd == "XX"
                in
                if hasXYear || hasXMonth || hasXDay then
                    maskedToInterval yMask (Just mm) (Just dd)

                else
                    problem "Not a masked date"
            )


maskedToInterval : String -> Maybe String -> Maybe String -> Parser EDTF
maskedToInterval yStr mStr dStr =
    let
        digitsPart =
            stripSign yStr

        hasMask =
            String.contains "X" digitsPart
                || (mStr == Just "XX")
                || (dStr == Just "XX")
    in
    if not hasMask then
        problem "Not a masked date"

    else
        case yearBoundsFromMasked yStr of
            Ok ( yLo, yHi ) ->
                let
                    ( mLo, mHi ) =
                        monthBounds mStr

                    lower =
                        buildLowerBound yLo mLo mStr dStr

                    upper =
                        buildUpperBound yHi mHi mStr dStr
                in
                succeed (Interval (Just lower) (Just upper))

            Err msg ->
                problem msg


buildLowerBound : Int -> Month -> Maybe String -> Maybe String -> DateAnnotated
buildLowerBound yLo mLo mStr dStr =
    let
        dateVal =
            case ( mStr, dStr ) of
                ( Just _, Nothing ) ->
                    YM yLo mLo

                ( Nothing, _ ) ->
                    Y yLo

                ( _, Just _ ) ->
                    let
                        d =
                            case dStr of
                                Just "XX" ->
                                    1

                                Just dd ->
                                    Maybe.withDefault 1 (String.toInt dd)

                                Nothing ->
                                    1
                    in
                    YMD yLo mLo d
    in
    { date = dateVal, uncertain = False, approximate = False }


buildUpperBound : Int -> Month -> Maybe String -> Maybe String -> DateAnnotated
buildUpperBound yHi mHi mStr dStr =
    let
        dateVal =
            case ( mStr, dStr ) of
                ( Just _, Nothing ) ->
                    YM yHi mHi

                ( Nothing, _ ) ->
                    Y yHi

                ( _, Just _ ) ->
                    let
                        d =
                            case dStr of
                                Just "XX" ->
                                    daysInMonth yHi mHi

                                Just dd ->
                                    Maybe.withDefault (daysInMonth yHi mHi) (String.toInt dd)

                                Nothing ->
                                    daysInMonth yHi mHi
                    in
                    YMD yHi mHi d
    in
    { date = dateVal, uncertain = False, approximate = False }


monthBounds : Maybe String -> ( Month, Month )
monthBounds mStr =
    case mStr of
        Just "XX" ->
            ( Jan, Dec )

        Just mm ->
            case String.toInt mm of
                Just m ->
                    ( monthFromInt m, monthFromInt m )

                Nothing ->
                    ( Jan, Dec )

        Nothing ->
            ( Jan, Dec )



-- DATE PARSERS


pDateAnnotated : Parser DateAnnotated
pDateAnnotated =
    pDateValue
        |> andThen
            (\dv ->
                pUAFlags
                    |> map (buildDateAnnotated dv)
            )


buildDateAnnotated : DateValue -> ( Bool, Bool ) -> DateAnnotated
buildDateAnnotated dv ( u, a ) =
    { date = dv, uncertain = u, approximate = a }


pUAFlags : Parser ( Bool, Bool )
pUAFlags =
    let
        step ( u, a ) =
            oneOf
                [ symbol "%" |> map (\_ -> Loop ( True, True ))
                , symbol "?" |> map (\_ -> Loop ( True, a ))
                , symbol "~" |> map (\_ -> Loop ( u, True ))
                , succeed (Done ( u, a ))
                ]
    in
    loop ( False, False ) step


pDateValue : Parser DateValue
pDateValue =
    oneOf
        [ pExpandedYear
        , P.backtrackable pSeason
        , pYmdOrYmOrY
        ]


{-| YYYY, YYYY-MM, YYYY-MM-DD (4-digit years only)
-}
pYmdOrYmOrY : Parser DateValue
pYmdOrYmOrY =
    signedInteger
        |> andThen
            (\s ->
                let
                    digits =
                        stripSign s
                in
                if String.length digits == 4 then
                    let
                        year =
                            String.toInt s |> Maybe.withDefault 0
                    in
                    oneOf
                        [ symbol "-"
                            |> andThen (\_ -> parseMonth2)
                            |> andThen
                                (\m ->
                                    oneOf
                                        [ symbol "-"
                                            |> andThen (\_ -> twoDigitInt 1 31)
                                            |> andThen
                                                (\d ->
                                                    if year == 1752 && m == Sep && (d > 2 && d < 14) then
                                                        problem "Those days do not exist"

                                                    else if d <= daysInMonth year m then
                                                        succeed (YMD year m d)

                                                    else
                                                        problem "Day out of range for month/year"
                                                )
                                        , succeed (YM year m)
                                        ]
                                )
                        , succeed (Y year)
                        ]

                else
                    problem "Expected 4-digit year (optionally signed) (or use expanded 'Y' form)"
            )


{-| Seasons: YYYY-21..24
-}
pSeason : Parser DateValue
pSeason =
    signedInteger
        |> andThen
            (\ys ->
                let
                    digits =
                        stripSign ys
                in
                if String.length digits == 4 then
                    let
                        year =
                            String.toInt ys |> Maybe.withDefault 0
                    in
                    symbol "-"
                        |> andThen (\_ -> parseSeasonCode year)

                else
                    problem "Expected 4-digit year before season code"
            )


parseSeasonCode : Int -> Parser DateValue
parseSeasonCode year =
    oneOf
        [ keyword "21" |> map (\_ -> Season year Spring)
        , keyword "22" |> map (\_ -> Season year Summer)
        , keyword "23" |> map (\_ -> Season year Autumn)
        , keyword "24" |> map (\_ -> Season year Winter)
        ]


{-| Expanded year: 'Y' + signed integer (>= 5 digits recommended)
-}
pExpandedYear : Parser DateValue
pExpandedYear =
    symbol "Y"
        |> andThen (\_ -> signedInteger)
        |> andThen
            (\numStr ->
                case String.toInt numStr of
                    Just n ->
                        succeed (Y n)

                    Nothing ->
                        problem "Invalid expanded year number"
            )



-- PARSER HELPERS


parseYearDigits : Parser YearDigits
parseYearDigits =
    signString
        |> andThen
            (\sgn ->
                year4String
                    |> map (\digits -> { sign = sgn, digits = digits })
            )


signString : Parser String
signString =
    oneOf [ getChompedString (chompIf isSign), succeed "" ]


signedInteger : Parser String
signedInteger =
    getChompedString
        (oneOf
            [ chompIf isSign |> andThen (\_ -> chompWhile Char.isDigit)
            , chompWhile Char.isDigit
            ]
        )


year4String : Parser String
year4String =
    getChompedString (chompWhile (\c -> Char.isDigit c || c == 'X'))
        |> andThen
            (\yyyy ->
                if String.length yyyy == 4 && String.all (\c -> Char.isDigit c || c == 'X') yyyy then
                    succeed yyyy

                else
                    problem "Expected 4 of [0-9X] for year"
            )


monthString : Parser String
monthString =
    getChompedString (chompWhile (\c -> Char.isDigit c || c == 'X'))
        |> andThen
            (\mm ->
                if isValidMonthOrXX mm then
                    succeed mm

                else
                    problem "Invalid month or 'XX'"
            )


dayString : Parser String
dayString =
    getChompedString (chompWhile (\c -> Char.isDigit c || c == 'X'))
        |> andThen
            (\dd ->
                if isValidDayOrXX dd then
                    succeed dd

                else
                    problem "Invalid day or 'XX'"
            )


twoDigitInt : Int -> Int -> Parser Int
twoDigitInt lo hi =
    getChompedString (chompWhile Char.isDigit)
        |> andThen
            (\s ->
                if String.length s == 2 then
                    case String.toInt s of
                        Just n ->
                            if n >= lo && n <= hi then
                                succeed n

                            else
                                problem "Out-of-range number"

                        Nothing ->
                            problem "Invalid number"

                else
                    problem "Expected two digits"
            )


parseMonth2 : Parser Month
parseMonth2 =
    twoDigitInt 1 12
        |> map monthFromInt


monthFromInt : Int -> Month
monthFromInt n =
    case n of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May_

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May_ ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\t')
        |> map (\_ -> ())



-- VALIDATION


isSign : Char -> Bool
isSign c =
    c == '+' || c == '-'


isValidMonthOrXX : String -> Bool
isValidMonthOrXX s =
    (s == "XX")
        || (String.length s
                == 2
                && (case String.toInt s of
                        Just n ->
                            n >= 1 && n <= 12

                        Nothing ->
                            False
                   )
           )


isValidDayOrXX : String -> Bool
isValidDayOrXX s =
    (s == "XX")
        || (String.length s
                == 2
                && (case String.toInt s of
                        Just n ->
                            n >= 1 && n <= 31

                        Nothing ->
                            False
                   )
           )



-- MASKED (X) SUPPORT


yearBoundsFromMasked : String -> Result String ( Int, Int )
yearBoundsFromMasked ystr =
    let
        ( sign, digits ) =
            if String.startsWith "-" ystr || String.startsWith "+" ystr then
                ( String.left 1 ystr, String.dropLeft 1 ystr )

            else
                ( "", ystr )

        padMin c =
            if c == 'X' then
                '0'

            else
                c

        padMax c =
            if c == 'X' then
                '9'

            else
                c
    in
    if String.length digits == 4 && String.all (\c -> Char.isDigit c || c == 'X') digits then
        let
            minStr =
                sign ++ String.fromList (List.map padMin (String.toList digits))

            maxStr =
                sign ++ String.fromList (List.map padMax (String.toList digits))
        in
        case ( String.toInt minStr, String.toInt maxStr ) of
            ( Just lo, Just hi ) ->
                Ok ( lo, hi )

            _ ->
                Err "Invalid masked year"

    else
        Err "Expected 4 characters of digits or X for year"


stripSign : String -> String
stripSign s =
    if String.startsWith "-" s || String.startsWith "+" s then
        String.dropLeft 1 s

    else
        s



-- DATE UTILITIES


daysInMonth : Int -> Month -> Int
daysInMonth y m =
    case m of
        Jan ->
            31

        Feb ->
            let
                leap =
                    (modBy 400 y == 0) || ((modBy 4 y == 0) && (modBy 100 y /= 0))
            in
            if leap then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May_ ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            if y == 1752 then
                19

            else
                30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31



-- STRING CONVERSION


maybeAnnotatedToString : Maybe DateAnnotated -> String
maybeAnnotatedToString m =
    case m of
        Just d ->
            dateAnnotatedToString d

        Nothing ->
            ".."


dateAnnotatedToString : DateAnnotated -> String
dateAnnotatedToString da =
    let
        base =
            case da.date of
                YMD y m d ->
                    formatYear y ++ "-" ++ pad2 (monthToInt m) ++ "-" ++ pad2 d

                YM y m ->
                    formatYear y ++ "-" ++ pad2 (monthToInt m)

                Y y ->
                    formatYear y

                Season y s ->
                    formatYear y ++ "-" ++ seasonCode s

        suffix =
            if da.uncertain && da.approximate then
                "%"

            else
                uncertaintyMarker da.uncertain ++ approximationMarker da.approximate
    in
    base ++ suffix


formatYear : Int -> String
formatYear y =
    if abs y >= 10000 || y < 0 then
        "Y" ++ String.fromInt y

    else
        String.fromInt y
            |> String.pad 4 '0'


seasonCode : Season -> String
seasonCode s =
    case s of
        Spring ->
            "21"

        Summer ->
            "22"

        Autumn ->
            "23"

        Winter ->
            "24"


uncertaintyMarker : Bool -> String
uncertaintyMarker uncertain =
    if uncertain then
        "?"

    else
        ""


approximationMarker : Bool -> String
approximationMarker approximate =
    if approximate then
        "~"

    else
        ""


pad2 : Int -> String
pad2 n =
    String.fromInt n
        |> String.pad 2 '0'
