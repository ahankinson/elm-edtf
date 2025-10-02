module EDTFExamplesTest exposing (tests)

import EDTF exposing (DateValue(..), EDTF(..), Month(..), Season(..), parse, toString)
import Expect
import Test exposing (Test, describe, test)


{-| A test suite that mirrors the structure of the provided Python
`EXAMPLES`, but adapted to what the current parser supports.

Notes:

  - We assert AST structure (and sometimes `toString`) for the supported subset.
  - For features not implemented yet (times, masked digits `X`, sets, `%`,
    L2 seasons/quarters, exponential years), we assert `Err` for now so the
    suite documents the gaps.
  - When Python examples specify _derived_ lower/upper bounds, we simply
    check that parsing succeeds (the library does not yet compute bounds).
  - Unknown-start (`/YYYY..`) and unknown-end (`YYYY..-/`) interval shorthands
    are covered and should parse successfully.
  - BCE (negative) years are accepted in 4-digit form and expanded `Y` form.

-}
tests : Test
tests =
    describe "EDTF EXAMPLES subset (current parser)"
        [ describe "Level 0"
            [ test "YYYY-MM-DD"
                (\_ ->
                    Expect.equal (Ok (Single { date = YMD 2001 Feb 3, uncertain = False, approximate = False }))
                        (parse "2001-02-03")
                )
            , test "YYYY-MM"
                (\_ ->
                    Expect.equal (Ok (Single { date = YM 2008 Dec, uncertain = False, approximate = False }))
                        (parse "2008-12")
                )
            , test "YYYY"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 2008, uncertain = False, approximate = False }))
                        (parse "2008")
                )
            , test "negative year -0999"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y -999, uncertain = False, approximate = False }))
                        (parse "-0999")
                )
            , test "year zero 0000"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 0, uncertain = False, approximate = False }))
                        (parse "0000")
                )
            , test "DateTime not supported (T/Z)"
                (\_ ->
                    case parse "2001-02-03T09:30:01" of
                        Ok _ ->
                            Expect.fail "Time parsing not implemented; should Err"

                        Err _ ->
                            Expect.pass
                )
            , test "DateTime with Z not supported"
                (\_ ->
                    case parse "2004-01-01T10:10:10Z" of
                        Ok _ ->
                            Expect.fail "Time parsing not implemented; should Err"

                        Err _ ->
                            Expect.pass
                )
            , test "DateTime with offset not supported"
                (\_ ->
                    case parse "2004-01-01T10:10:10+05:00" of
                        Ok _ ->
                            Expect.fail "Time parsing not implemented; should Err"

                        Err _ ->
                            Expect.pass
                )
            ]
        , describe "Level 0 intervals"
            [ test "year precision 1964/2008"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = Y 1964, uncertain = False, approximate = False }) (Just { date = Y 2008, uncertain = False, approximate = False })))
                        (parse "1964/2008")
                )
            , test "month precision 2004-06/2006-08"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = YM 2004 Jun, uncertain = False, approximate = False }) (Just { date = YM 2006 Aug, uncertain = False, approximate = False })))
                        (parse "2004-06/2006-08")
                )
            , test "day precision 2004-02-01/2005-02-08"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = YMD 2004 Feb 1, uncertain = False, approximate = False }) (Just { date = YMD 2005 Feb 8, uncertain = False, approximate = False })))
                        (parse "2004-02-01/2005-02-08")
                )
            , test "mixed precision end month"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = YMD 2004 Feb 1, uncertain = False, approximate = False }) (Just { date = YM 2005 Feb, uncertain = False, approximate = False })))
                        (parse "2004-02-01/2005-02")
                )
            , test "mixed precision end year"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = YMD 2004 Feb 1, uncertain = False, approximate = False }) (Just { date = Y 2005, uncertain = False, approximate = False })))
                        (parse "2004-02-01/2005")
                )
            , test "start year to end month"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = Y 2005, uncertain = False, approximate = False }) (Just { date = YM 2006 Feb, uncertain = False, approximate = False })))
                        (parse "2005/2006-02")
                )
            , test "BCE interval -2005/-1999-02"
                (\_ ->
                    Expect.equal (Ok (Interval (Just { date = Y -2005, uncertain = False, approximate = False }) (Just { date = YM -1999 Feb, uncertain = False, approximate = False })))
                        (parse "-2005/-1999-02")
                )
            ]
        , describe "Level 1 flags (subset)"
            [ test "uncertain year 1984?"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 1984, uncertain = True, approximate = False }))
                        (parse "1984?")
                )
            , test "approx year 1984~"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 1984, uncertain = False, approximate = True }))
                        (parse "1984~")
                )
            , test "uncertain and approximate % is supported"
                (\_ ->
                    case parse "1984%" of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "% (circa with wider delta) not implemented; should Err"
                )
            , test "toString uses % when both uncertain and approximate"
                (\_ ->
                    case parse "1984%" of
                        Ok edtf ->
                            Expect.equal "1984%" (toString edtf)

                        Err e ->
                            Expect.fail (Debug.toString e)
                )
            ]
        , describe "Unknown start/end shorthands"
            [ test "/2006"
                (\_ ->
                    case parse "/2006" of
                        Ok (Interval Nothing (Just _)) ->
                            Expect.pass

                        other ->
                            Expect.fail ("Expected Interval Nothing (Just _), got: " ++ Debug.toString other)
                )
            , test "2004-06-01/"
                (\_ ->
                    case parse "2004-06-01/" of
                        Ok (Interval (Just _) Nothing) ->
                            Expect.pass

                        other ->
                            Expect.fail ("Expected Interval (Just _) Nothing, got: " ++ Debug.toString other)
                )
            , test "../2006 (open start token)"
                (\_ ->
                    case parse "../2006" of
                        Ok (Interval Nothing (Just _)) ->
                            Expect.pass

                        other ->
                            Expect.fail ("Expected Interval Nothing (Just _), got: " ++ Debug.toString other)
                )
            , test "2004-01-01/.. (open end token)"
                (\_ ->
                    case parse "2004-01-01/.." of
                        Ok (Interval (Just _) Nothing) ->
                            Expect.pass

                        other ->
                            Expect.fail ("Expected Interval (Just _) Nothing, got: " ++ Debug.toString other)
                )
            ]
        , describe "Expanded and long years"
            [ test "expanded Y170000002"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 170000002, uncertain = False, approximate = False }))
                        (parse "Y170000002")
                )
            , test "expanded Y-170000002"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y -170000002, uncertain = False, approximate = False }))
                        (parse "Y-170000002")
                )
            , test "exponential Y17E7 not supported"
                (\_ ->
                    case parse "Y17E7" of
                        Ok _ ->
                            Expect.fail "Exponential form not implemented; should Err"

                        Err _ ->
                            Expect.pass
                )
            ]
        , describe "Seasons"
            [ test "2001-21 (Spring)"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season 2001 Spring, uncertain = False, approximate = False }))
                        (parse "2001-21")
                )
            , test "2010-24 (Winter)"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season 2010 Winter, uncertain = False, approximate = False }))
                        (parse "2010-24")
                )
            , test "L2 seasons 29/34 not supported"
                (\_ ->
                    case ( parse "2001-29", parse "2001-34" ) of
                        ( Err _, Err _ ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "L2 seasons should Err"
                )
            ]
        , describe "Leap years and other anomalies"
            [ test "2000-02-29"
                (\_ ->
                    Expect.equal (Ok (Single { date = YMD 2000 Feb 29, uncertain = False, approximate = False }))
                        (parse "2000-02-29")
                )
            , test "2001-02-29"
                (\_ ->
                    case parse "2001-02-29" of
                        Ok _ ->
                            Expect.fail "Should not parse non-leap-year dates"

                        Err _ ->
                            Expect.pass
                )
            , test "1752-09-03"
                (\_ ->
                    case parse "1752-09-03" of
                        Ok _ ->
                            Expect.fail "September 3, 1752 does not exist."

                        Err _ ->
                            Expect.pass
                )
            , test "1752-09-02"
                (\_ ->
                    case parse "1752-09-02" of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "September 2, 1752 does exist."
                )
            , test "1752-09-14"
                (\_ ->
                    case parse "1752-09-14" of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "September 14, 1752 does not exist."
                )
            ]
        , describe "Unsupported (documented gaps)"
            [ test "masked digits X"
                (\_ ->
                    case parse "199X" of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "Masked digits not implemented; should Err"
                )
            , test "sets and lists []/{}"
                (\_ ->
                    case ( parse "[1667, 1760-12]", parse "{1960, 1961-12}" ) of
                        ( Err _, Err _ ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Sets should Err"
                )
            ]
        ]
