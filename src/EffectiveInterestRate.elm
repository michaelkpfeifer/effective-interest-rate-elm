module EffectiveInterestRate exposing
    ( NormalizedPayment
    , NormalizedPaymentStream
    , Payment
    , PaymentStream
    , dayInYear
    , earliestPayment
    , effectiveInterestRate
    , isInLeapYear
    , isLeapYear
    , netPresentValue
    , netPresentValueDerivative
    , normalizedDayInYear
    , toNormalizedPayment
    , toNormalizedPaymentStream
    )

import Date
import List.Min2Items
import NewtonIteration


type alias Payment =
    { amount : Float
    , date : Date.Date
    }


type alias PaymentStream =
    List.Min2Items.Min2Items Payment


type alias NormalizedPayment =
    { amount : Float
    , offset : Float
    }


type alias NormalizedPaymentStream =
    List.Min2Items.Min2Items NormalizedPayment


earliestPayment : PaymentStream -> Payment
earliestPayment paymentStream =
    List.Min2Items.minimumWith
        (\p1 p2 -> Date.compare p1.date p2.date)
        paymentStream


dayInYear : Date.Date -> Int
dayInYear date =
    Date.ordinalDay date - 1


isLeapYear : Int -> Bool
isLeapYear year =
    let
        divisibleBy : Int -> Bool
        divisibleBy n =
            remainderBy n year == 0
    in
    divisibleBy 4 && (not (divisibleBy 100) || divisibleBy 400)


isInLeapYear : Date.Date -> Bool
isInLeapYear date =
    Date.year date |> isLeapYear


normalizedDayInYear : Date.Date -> Float
normalizedDayInYear date =
    if isInLeapYear date then
        toFloat (dayInYear date) / 366

    else
        toFloat (dayInYear date) / 365


toNormalizedPayment : Payment -> Payment -> NormalizedPayment
toNormalizedPayment referencePayment payment =
    { amount = payment.amount
    , offset =
        toFloat (Date.year payment.date)
            - toFloat (Date.year referencePayment.date)
            + normalizedDayInYear payment.date
            - normalizedDayInYear referencePayment.date
    }


toNormalizedPaymentStream : PaymentStream -> NormalizedPaymentStream
toNormalizedPaymentStream paymentStream =
    let
        referencePayment : Payment
        referencePayment =
            earliestPayment paymentStream
    in
    List.Min2Items.map (\payment -> toNormalizedPayment referencePayment payment) paymentStream


netPresentValue : NormalizedPaymentStream -> (Float -> Float)
netPresentValue normalizedPaymentStream =
    \x ->
        List.Min2Items.foldl
            (\np sum -> sum + np.amount * (1 + x) ^ -np.offset)
            0.0
            normalizedPaymentStream


netPresentValueDerivative : NormalizedPaymentStream -> (Float -> Float)
netPresentValueDerivative normalizedpaymentstream =
    \x ->
        List.Min2Items.foldl
            (\np sum -> sum + np.amount * -np.offset * (1 + x) ^ (-np.offset - 1))
            0.0
            normalizedpaymentstream


effectiveInterestRate : PaymentStream -> Result String Float
effectiveInterestRate paymentStream =
    let
        normalizedPaymentStream : NormalizedPaymentStream
        normalizedPaymentStream =
            toNormalizedPaymentStream paymentStream

        startValue : Float
        startValue =
            -0.75

        maxIterationDifference : Float
        maxIterationDifference =
            1.0e-8

        maxIterations : Int
        maxIterations =
            64
    in
    NewtonIteration.iterate
        (netPresentValue normalizedPaymentStream)
        (netPresentValueDerivative normalizedPaymentStream)
        startValue
        maxIterationDifference
        maxIterations
