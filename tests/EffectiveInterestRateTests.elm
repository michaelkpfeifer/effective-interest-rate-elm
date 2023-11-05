module EffectiveInterestRateTests exposing
    ( dayInYearTest
    , earliestPaymentTest
    , effectiveInterestRateTest
    , isInLeapYearTest
    , isLeapYearTest
    , netPresentValueDerivativeTest
    , netPresentValueTest
    , normalizedDayInYearTest
    , toNormalizedPaymentStreamTest
    , toNormalizedPaymentTest
    )

import Date
import EffectiveInterestRate
import Expect
import List.Min2Items
import Test
import Time


listProduct : List a -> List b -> List ( a, b )
listProduct la lb =
    List.map (\va -> List.map (\vb -> ( va, vb )) lb) la
        |> List.concat


earliestPaymentTest : Test.Test
earliestPaymentTest =
    Test.describe "EffectiveInterestRate.earliestPayment"
        [ Test.test "the earliest payment of a stream of payments is the earliest one"
            (\_ ->
                let
                    earliestPayment : EffectiveInterestRate.Payment
                    earliestPayment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    somePayment : EffectiveInterestRate.Payment
                    somePayment =
                        { amount = 500, date = Date.fromCalendarDate 2020 Time.Jul 1 }

                    newestPayment : EffectiveInterestRate.Payment
                    newestPayment =
                        { amount = 500, date = Date.fromCalendarDate 2021 Time.Jan 1 }

                    paymentStream : EffectiveInterestRate.PaymentStream
                    paymentStream =
                        List.Min2Items.Min2Items newestPayment somePayment [ earliestPayment ]
                in
                Expect.equal (EffectiveInterestRate.earliestPayment paymentStream) earliestPayment
            )
        ]


dayInYearTest : Test.Test
dayInYearTest =
    Test.describe "EffectiveInterestRate.dayInYear"
        [ Test.test "enumerating days starts at 0"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2020 Time.Jan 1
                in
                Expect.equal (EffectiveInterestRate.dayInYear date) 0
            )
        , Test.test "the index of the last day of a non-leap year is 364"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2021 Time.Dec 31
                in
                Expect.equal (EffectiveInterestRate.dayInYear date) 364
            )
        , Test.test "the index of the last day of a leap year is 365"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2020 Time.Dec 31
                in
                Expect.equal (EffectiveInterestRate.dayInYear date) 365
            )
        ]


isLeapYearTest : Test.Test
isLeapYearTest =
    Test.describe "EffectiveInterestRate.isLeapYear"
        [ Test.test "2021 is not a leap year"
            (\_ -> Expect.equal (EffectiveInterestRate.isLeapYear 2021) False)
        , Test.test "2020 is a leap year"
            (\_ -> Expect.equal (EffectiveInterestRate.isLeapYear 2020) True)
        , Test.test "2100 is not a leap year"
            (\_ -> Expect.equal (EffectiveInterestRate.isLeapYear 2100) False)
        , Test.test "2400 is a leap year"
            (\_ -> Expect.equal (EffectiveInterestRate.isLeapYear 2400) True)
        ]


isInLeapYearTest : Test.Test
isInLeapYearTest =
    Test.describe "EffectiveInterestRate.isInLeapYear"
        [ Test.test "Feb 01, 2021 is not in a leap year"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2021 Time.Feb 1
                in
                Expect.equal (EffectiveInterestRate.isInLeapYear date) False
            )
        , Test.test "Feb 01, 2020 is in a leap year"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2020 Time.Feb 1
                in
                Expect.equal (EffectiveInterestRate.isInLeapYear date) True
            )
        ]


normalizedDayInYearTest : Test.Test
normalizedDayInYearTest =
    Test.describe "EffectiveInterestRate.normalizedDayInYear"
        [ Test.test "Jan 01 is day 0.0"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2021 Time.Jan 1
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.normalizedDayInYear date)
                    0.0
            )
        , Test.test "Dec 31 in a non-leap year is day 364/365"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2021 Time.Dec 31
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.normalizedDayInYear date)
                    (364.0 / 365.0)
            )
        , Test.test "Dec 31 in a leap year is day 364/365"
            (\_ ->
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate 2020 Time.Dec 31
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.normalizedDayInYear date)
                    (365.0 / 366.0)
            )
        ]


toNormalizedPaymentTest : Test.Test
toNormalizedPaymentTest =
    Test.describe "EffectiveInterestRate.toRelativePayment"
        [ Test.test "offset is 0 if payment and reference payment are equal"
            (\_ ->
                let
                    referencePayment : EffectiveInterestRate.Payment
                    referencePayment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    payment : EffectiveInterestRate.Payment
                    payment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Jan 1 }
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.toNormalizedPayment referencePayment payment).offset
                    0.0
            )
        , Test.test "offset between first and last day in a non-leap year is 364/365"
            (\_ ->
                let
                    referencePayment : EffectiveInterestRate.Payment
                    referencePayment =
                        { amount = 1000, date = Date.fromCalendarDate 2021 Time.Jan 1 }

                    payment : EffectiveInterestRate.Payment
                    payment =
                        { amount = 1000, date = Date.fromCalendarDate 2021 Time.Dec 31 }
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.toNormalizedPayment referencePayment payment).offset
                    (364.0 / 365.0)
            )
        , Test.test "offset between first and last day in a non-leap year is 365/366"
            (\_ ->
                let
                    referencePayment : EffectiveInterestRate.Payment
                    referencePayment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    payment : EffectiveInterestRate.Payment
                    payment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Dec 31 }
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.toNormalizedPayment referencePayment payment).offset
                    (365.0 / 366.0)
            )
        , Test.test "Jan 01 to the next Jan 01 is one full year"
            (\_ ->
                let
                    referencePayment : EffectiveInterestRate.Payment
                    referencePayment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    payment : EffectiveInterestRate.Payment
                    payment =
                        { amount = 1000, date = Date.fromCalendarDate 2021 Time.Jan 1 }
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.toNormalizedPayment referencePayment payment).offset
                    1.0
            )
        , Test.test "leap years are are respected across year boundaries"
            (\_ ->
                let
                    referencePayment : EffectiveInterestRate.Payment
                    referencePayment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Dec 31 }

                    payment : EffectiveInterestRate.Payment
                    payment =
                        { amount = 1000, date = Date.fromCalendarDate 2021 Time.Jan 2 }
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.toNormalizedPayment referencePayment payment).offset
                    ((1.0 / 365.0) + (1.0 / 366.0))
            )
        , Test.test "February in a leap year has 29 days"
            (\_ ->
                let
                    referencePayment : EffectiveInterestRate.Payment
                    referencePayment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Feb 1 }

                    payment : EffectiveInterestRate.Payment
                    payment =
                        { amount = 1000, date = Date.fromCalendarDate 2020 Time.Feb 29 }
                in
                Expect.within
                    (Expect.Absolute 1.0e-8)
                    (EffectiveInterestRate.toNormalizedPayment referencePayment payment).offset
                    (28.0 / 366.0)
            )
        ]


toNormalizedPaymentStreamTest : Test.Test
toNormalizedPaymentStreamTest =
    Test.describe "EffectiveInterestRate.toNormalizedPaymentStream"
        [ Test.test "it normalizes a payment stream of three payments"
            (\_ ->
                let
                    paymentJan01 : EffectiveInterestRate.Payment
                    paymentJan01 =
                        { amount = -1000, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    paymentJan02 : EffectiveInterestRate.Payment
                    paymentJan02 =
                        { amount = 500, date = Date.fromCalendarDate 2020 Time.Jan 2 }

                    paymentDec31 : EffectiveInterestRate.Payment
                    paymentDec31 =
                        { amount = 500, date = Date.fromCalendarDate 2020 Time.Dec 31 }

                    normalizedPaymentStream : EffectiveInterestRate.NormalizedPaymentStream
                    normalizedPaymentStream =
                        List.Min2Items.Min2Items paymentJan01 paymentJan02 [ paymentDec31 ]
                            |> EffectiveInterestRate.toNormalizedPaymentStream
                in
                Expect.all
                    [ \nps ->
                        Expect.within
                            (Expect.Absolute 1.0e-8)
                            (0.0 / 1.0)
                            (List.Min2Items.first nps).offset
                    , \nps ->
                        Expect.within
                            (Expect.Absolute 1.0e-8)
                            (1.0 / 366.0)
                            (List.Min2Items.second nps).offset
                    , \nps ->
                        Expect.within
                            (Expect.Absolute 1.0e-8)
                            (365.0 / 366.0)
                            (List.Min2Items.rest nps
                                |> List.head
                                |> Maybe.withDefault { amount = 0, offset = 0 }
                                |> .offset
                            )
                    ]
                    normalizedPaymentStream
            )
        ]


netPresentValueTest : Test.Test
netPresentValueTest =
    Test.describe "EffectiveInterestRate.netPresentValue"
        [ Test.test "net present value for an interest rate of 0 is equal to the sum of amounts"
            (\_ ->
                let
                    payment1 : EffectiveInterestRate.Payment
                    payment1 =
                        { amount = -1000, date = Date.fromCalendarDate 2019 Time.Jan 1 }

                    payment2 : EffectiveInterestRate.Payment
                    payment2 =
                        { amount = 1600, date = Date.fromCalendarDate 2019 Time.Apr 4 }

                    payment3 : EffectiveInterestRate.Payment
                    payment3 =
                        { amount = -2000, date = Date.fromCalendarDate 2019 Time.Jul 7 }

                    payment4 : EffectiveInterestRate.Payment
                    payment4 =
                        { amount = 1600, date = Date.fromCalendarDate 2019 Time.Oct 10 }

                    normalizedPaymentStream : EffectiveInterestRate.NormalizedPaymentStream
                    normalizedPaymentStream =
                        List.Min2Items.Min2Items payment1 payment2 [ payment3, payment4 ]
                            |> EffectiveInterestRate.toNormalizedPaymentStream

                    npv : Float -> Float
                    npv =
                        EffectiveInterestRate.netPresentValue normalizedPaymentStream
                in
                Expect.within (Expect.Absolute 1.0e-8) (npv 0.0) 200.0
            )
        , Test.test "returns the correct manually computed result"
            (\_ ->
                let
                    payment1 : EffectiveInterestRate.Payment
                    payment1 =
                        { amount = -1000, date = Date.fromCalendarDate 2019 Time.Jan 1 }

                    payment2 : EffectiveInterestRate.Payment
                    payment2 =
                        { amount = 500, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    payment3 : EffectiveInterestRate.Payment
                    payment3 =
                        { amount = 500, date = Date.fromCalendarDate 2021 Time.Jan 1 }

                    normalizedPaymentStream : EffectiveInterestRate.NormalizedPaymentStream
                    normalizedPaymentStream =
                        List.Min2Items.Min2Items payment1 payment2 [ payment3 ]
                            |> EffectiveInterestRate.toNormalizedPaymentStream

                    npv : Float -> Float
                    npv =
                        EffectiveInterestRate.netPresentValue normalizedPaymentStream
                in
                Expect.within (Expect.Absolute 1.0e-8) (npv 1.0) -625.0
            )
        ]


netPresentValueDerivativeTest : Test.Test
netPresentValueDerivativeTest =
    Test.describe "EffectiveInterestRate.netPresentValueDerivative"
        [ Test.test "returns the correct manually computed result"
            (\_ ->
                let
                    payment1 : EffectiveInterestRate.Payment
                    payment1 =
                        { amount = -1000, date = Date.fromCalendarDate 2019 Time.Jan 1 }

                    payment2 : EffectiveInterestRate.Payment
                    payment2 =
                        { amount = 500, date = Date.fromCalendarDate 2020 Time.Jan 1 }

                    payment3 : EffectiveInterestRate.Payment
                    payment3 =
                        { amount = 500, date = Date.fromCalendarDate 2021 Time.Jan 1 }

                    normalizedPaymentStream : EffectiveInterestRate.NormalizedPaymentStream
                    normalizedPaymentStream =
                        List.Min2Items.Min2Items payment1 payment2 [ payment3 ]
                            |> EffectiveInterestRate.toNormalizedPaymentStream

                    npvp : Float -> Float
                    npvp =
                        EffectiveInterestRate.netPresentValueDerivative normalizedPaymentStream
                in
                Expect.within (Expect.Absolute 1.0e-8) (npvp 1.0) -250.0
            )
        ]


effectiveInterestRateTest : Test.Test
effectiveInterestRateTest =
    Test.describe "EffectiveInterestRate.effectiveInterestRate"
        [ Test.test "it is 0 in a very simple case"
            (\_ ->
                let
                    payment1 : EffectiveInterestRate.Payment
                    payment1 =
                        { amount = 2000, date = Date.fromCalendarDate 2013 Time.Jun 1 }

                    payment2 : EffectiveInterestRate.Payment
                    payment2 =
                        { amount = -1000, date = Date.fromCalendarDate 2014 Time.Jun 1 }

                    payment3 : EffectiveInterestRate.Payment
                    payment3 =
                        { amount = -1000, date = Date.fromCalendarDate 2015 Time.Jun 1 }

                    paymentStream : EffectiveInterestRate.PaymentStream
                    paymentStream =
                        List.Min2Items.Min2Items payment1 payment2 [ payment3 ]
                in
                case EffectiveInterestRate.effectiveInterestRate paymentStream of
                    Ok interestRate ->
                        Expect.within (Expect.Absolute 1.0e-8) interestRate 0.0

                    Err err ->
                        Expect.fail err
            )
        , Test.test "it has the expected sign in a simple case"
            (\_ ->
                let
                    payment1 : EffectiveInterestRate.Payment
                    payment1 =
                        { amount = 2000, date = Date.fromCalendarDate 2013 Time.Jun 1 }

                    payment2 : EffectiveInterestRate.Payment
                    payment2 =
                        { amount = -1000, date = Date.fromCalendarDate 2014 Time.Jun 1 }

                    payment3 : EffectiveInterestRate.Payment
                    payment3 =
                        { amount = -1000, date = Date.fromCalendarDate 2015 Time.Jun 1 }

                    payment4 : EffectiveInterestRate.Payment
                    payment4 =
                        { amount = -100, date = Date.fromCalendarDate 2015 Time.Jul 1 }

                    paymentStream : EffectiveInterestRate.PaymentStream
                    paymentStream =
                        List.Min2Items.Min2Items payment1 payment2 [ payment3, payment4 ]
                in
                case EffectiveInterestRate.effectiveInterestRate paymentStream of
                    Ok interestRate ->
                        Expect.greaterThan 0.0 interestRate

                    Err err ->
                        Expect.fail err
            )
        , Test.test "it returns the expected value for simple real life case"
            (\_ ->
                let
                    payment1 : EffectiveInterestRate.Payment
                    payment1 =
                        { amount = -1065.25, date = Date.fromCalendarDate 2011 Time.Apr 21 }

                    payment2 : EffectiveInterestRate.Payment
                    payment2 =
                        { amount = 130.69, date = Date.fromCalendarDate 2014 Time.May 23 }

                    paymentStream : EffectiveInterestRate.PaymentStream
                    paymentStream =
                        List.Min2Items.Min2Items payment1 payment2 []
                in
                case EffectiveInterestRate.effectiveInterestRate paymentStream of
                    Ok interestRate ->
                        Expect.within (Expect.Absolute 1.0e-3) interestRate -0.4931

                    Err err ->
                        Expect.fail err
            )
        , Test.test "it returns the expected value for a stream of monthly payments"
            (\_ ->
                let
                    years : List Int
                    years =
                        List.range 2015 2034

                    months : List Time.Month
                    months =
                        [ Time.Jan
                        , Time.Feb
                        , Time.Mar
                        , Time.Apr
                        , Time.May
                        , Time.Jun
                        , Time.Jul
                        , Time.Aug
                        , Time.Sep
                        , Time.Oct
                        , Time.Nov
                        , Time.Dec
                        ]

                    paymentStream : EffectiveInterestRate.PaymentStream
                    paymentStream =
                        List.Min2Items.Min2Items
                            { amount = 240000, date = Date.fromCalendarDate 2015 Time.Jan 1 }
                            { amount = 0, date = Date.fromCalendarDate 2015 Time.Jan 1 }
                            (listProduct years months
                                |> List.map
                                    (\( year, month ) ->
                                        { amount = -1200, date = Date.fromCalendarDate year month 1 }
                                    )
                            )
                in
                case EffectiveInterestRate.effectiveInterestRate paymentStream of
                    Ok interestRate ->
                        Expect.within (Expect.Absolute 1.0e-3) interestRate (1.91 / 100.0)

                    Err err ->
                        Expect.fail err
            )
        ]
