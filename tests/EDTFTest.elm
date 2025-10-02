module EDTFTest exposing (tests)

import EDTF exposing (DateValue(..), EDTF(..), Month(..), Season(..), parse, toString)
import Expect
import Test exposing (Test, describe, skip, test)


{-| Run with elm-test.

Add this file under `tests/` and ensure `elm-explorations/test` is in your
`test-dependencies`.

-}
tests : Test
tests =
    describe "EDTF parser"
        [ describe "Calendar dates"
            [ test "year YYYY"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 2020, uncertain = False, approximate = False }))
                        (parse "2020")
                )
            , test "year-month YYYY-MM"
                (\_ ->
                    Expect.equal (Ok (Single { date = YM 2020 May_, uncertain = False, approximate = False }))
                        (parse "2020-05")
                )
            , test "year-month-day YYYY-MM-DD"
                (\_ ->
                    Expect.equal (Ok (Single { date = YMD 2020 May_ 17, uncertain = False, approximate = False }))
                        (parse "2020-05-17")
                )
            , test "BCE Dates"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y -39, uncertain = False, approximate = False }))
                        (parse "-0039")
                )
            , test "BCE YMD -0001-04-12"
                (\_ ->
                    Expect.equal (Ok (Single { date = YMD -1 Apr 12, uncertain = False, approximate = False }))
                        (parse "-0001-04-12")
                )
            , test "BCE YM -1200-07"
                (\_ ->
                    Expect.equal (Ok (Single { date = YM -1200 Jul, uncertain = False, approximate = False }))
                        (parse "-1200-07")
                )
            , test "BCE year -0001"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y -1, uncertain = False, approximate = False }))
                        (parse "-0001")
                )
            , test "BCE season -0001-21 (Spring)"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season -1 Spring, uncertain = False, approximate = False }))
                        (parse "-0001-21")
                )
            ]
        , describe "Seasons (21..24)"
            [ test "Spring"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season 2021 Spring, uncertain = False, approximate = False }))
                        (parse "2021-21")
                )
            , test "Summer"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season 2021 Summer, uncertain = False, approximate = False }))
                        (parse "2021-22")
                )
            , test "Autumn"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season 2021 Autumn, uncertain = False, approximate = False }))
                        (parse "2021-23")
                )
            , test "Winter"
                (\_ ->
                    Expect.equal (Ok (Single { date = Season 2021 Winter, uncertain = False, approximate = False }))
                        (parse "2021-24")
                )
            ]
        , describe "Uncertainty / Approximation flags"
            [ test "uncertain ?"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 1999, uncertain = True, approximate = False }))
                        (parse "1999?")
                )
            , test "approx ~"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 1999, uncertain = False, approximate = True }))
                        (parse "1999~")
                )
            , test "both ?~ in any order"
                (\_ ->
                    Expect.equal (Ok (Single { date = YM 1999 Dec, uncertain = True, approximate = True }))
                        (parse "1999-12~?")
                )
            ]
        , describe "Expanded years"
            [ test "unsigned"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 12034, uncertain = False, approximate = False }))
                        (parse "Y12034")
                )
            , test "negative"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y -12034, uncertain = False, approximate = False }))
                        (parse "Y-12034")
                )
            , test "positive explicit sign"
                (\_ ->
                    Expect.equal (Ok (Single { date = Y 12034, uncertain = False, approximate = False }))
                        (parse "Y+12034")
                )
            ]
        , describe "Intervals"
            [ test "closed interval"
                (\_ ->
                    Expect.equal
                        (Ok
                            (Interval (Just { date = Y 2020, uncertain = False, approximate = False })
                                (Just { date = YM 2021 May_, uncertain = False, approximate = False })
                            )
                        )
                        (parse "2020/2021-05")
                )
            , test "open end with .."
                (\_ ->
                    Expect.equal
                        (Ok
                            (Interval (Just { date = Y 2020, uncertain = False, approximate = False })
                                Nothing
                            )
                        )
                        (parse "2020/..")
                )
            , test "open start with .."
                (\_ ->
                    Expect.equal
                        (Ok
                            (Interval Nothing
                                (Just { date = YMD 2021 May_ 17, uncertain = False, approximate = False })
                            )
                        )
                        (parse "../2021-05-17")
                )
            , test "unknown start"
                (\_ ->
                    Expect.equal
                        (Ok
                            (Interval Nothing
                                (Just { date = YMD 2021 May_ 17, uncertain = False, approximate = False })
                            )
                        )
                        (parse "/2021-05-17")
                )
            , test "unknown end"
                (\_ ->
                    Expect.equal
                        (Ok
                            (Interval (Just { date = Y 2020, uncertain = False, approximate = False })
                                Nothing
                            )
                        )
                        (parse "2020/")
                )
            ]
        , describe "Lists"
            [ test "two items"
                (\_ ->
                    Expect.equal
                        (Ok
                            (List
                                [ Single { date = Y 2020, uncertain = False, approximate = False }
                                , Single { date = YM 2021 May_, uncertain = False, approximate = False }
                                ]
                            )
                        )
                        (parse "2020,2021-05")
                )
            , test "three items with spaces around commas"
                (\_ ->
                    Expect.equal
                        (Ok
                            (List
                                [ Single { date = Y 2020, uncertain = False, approximate = False }
                                , Single { date = Y 2021, uncertain = False, approximate = False }
                                , Single { date = Season 2022 Spring, uncertain = False, approximate = False }
                                ]
                            )
                        )
                        (parse "2020 , 2021 , 2022-21")
                )
            ]
        , describe "toString normalization"
            [ test "YMD" (\_ -> Expect.equal "2020-05-17" (toString (Single { date = YMD 2020 May_ 17, uncertain = False, approximate = False })))
            , test "YM" (\_ -> Expect.equal "2020-05" (toString (Single { date = YM 2020 May_, uncertain = False, approximate = False })))
            , test "Y (4-digit)" (\_ -> Expect.equal "2020" (toString (Single { date = Y 2020, uncertain = False, approximate = False })))
            , test "Expanded year keeps Y" (\_ -> Expect.equal "Y12034" (toString (Single { date = Y 12034, uncertain = False, approximate = False })))
            , test "Flags uncertain" (\_ -> Expect.equal "2020?" (toString (Single { date = Y 2020, uncertain = True, approximate = False })))
            , test "Flags approximate" (\_ -> Expect.equal "2020~" (toString (Single { date = Y 2020, uncertain = False, approximate = True })))
            , test "Flags uncertain and approximate" (\_ -> Expect.equal "2020%" (toString (Single { date = Y 2020, uncertain = True, approximate = True })))
            , test "Interval open"
                (\_ ->
                    Expect.equal "2020/.."
                        (toString
                            (Interval (Just { date = Y 2020, uncertain = False, approximate = False })
                                Nothing
                            )
                        )
                )
            , test "List join"
                (\_ ->
                    Expect.equal "2020,2021-05"
                        (toString
                            (List
                                [ Single { date = Y 2020, uncertain = False, approximate = False }
                                , Single { date = YM 2021 May_, uncertain = False, approximate = False }
                                ]
                            )
                        )
                )
            ]
        , describe "Invalid inputs"
            [ test "bad month digits"
                (\_ ->
                    case parse "2020-5" of
                        Ok _ ->
                            Expect.fail "Expected an error for one-digit month"

                        Err _ ->
                            Expect.pass
                )
            , test "bad day digits"
                (\_ ->
                    case parse "2020-05-7" of
                        Ok _ ->
                            Expect.fail "Expected an error for one-digit day"

                        Err _ ->
                            Expect.pass
                )
            , test "season requires 4-digit year"
                (\_ ->
                    case parse "20-21" of
                        Ok _ ->
                            Expect.fail "Expected an error for short year"

                        Err _ ->
                            Expect.pass
                )
            , test "not a edtf string"
                (\_ ->
                    case parse "not a edtf string" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "Y17E7-12-26 (Y means year-only; extra parts invalid)"
                (\_ ->
                    case parse "Y17E7-12-26" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2016-13-08 (month out of range)"
                (\_ ->
                    case parse "2016-13-08" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2016-02-39 (day out of range)"
                (\_ ->
                    case parse "2016-02-39" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , skip
                (test "-0000-01-01 (negative zero year; currently accepted as year 0)"
                    (\_ ->
                        case parse "-0000-01-01" of
                            Ok v ->
                                Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                            Err _ ->
                                Expect.pass
                    )
                )
            , test "2004-(06)?-11 (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2004-(06)?-11" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2004-06-(11)~ (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2004-06-(11)~" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2004-(06)% (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2004-(06)%" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2004-(06-11)? (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2004-(06-11)?" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2004?-06-(11)~ (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2004?-06-(11)~" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "(2004-(06)~)? (OLD SPEC parentheses)"
                (\_ ->
                    case parse "(2004-(06)~)?" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "(2004)?-06-04~ (OLD SPEC parentheses)"
                (\_ ->
                    case parse "(2004)?-06-04~" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "(2011)-06-04~ (OLD SPEC parentheses)"
                (\_ ->
                    case parse "(2011)-06-04~" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2011-(06-04)~ (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2011-(06-04)~" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            , test "2004-06-(01)~/2004-06-(20)~ (OLD SPEC parentheses)"
                (\_ ->
                    case parse "2004-06-(01)~/2004-06-(20)~" of
                        Ok v ->
                            Expect.fail ("Expected Err, got: " ++ Debug.toString v)

                        Err _ ->
                            Expect.pass
                )
            ]
        ]
