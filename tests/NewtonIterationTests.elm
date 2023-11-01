module NewtonIterationTests exposing (iterateTest)

import Expect
import NewtonIteration
import Test


iterateTest : Test.Test
iterateTest =
    Test.describe "NewtonIteration.iterate"
        [ Test.test "finds the root of the identity function"
            (\_ ->
                let
                    f : Float -> Float
                    f =
                        identity

                    fp : Float -> Float
                    fp =
                        always 1

                    result : Result String Float
                    result =
                        NewtonIteration.iterate f fp 1.0 1.0e-8 4
                in
                case result of
                    Ok root ->
                        Expect.within (Expect.Absolute 1.0e-8) root 0.0

                    Err err ->
                        Expect.fail err
            )
        , Test.test "does not find a root of x^4 - 1 with an accuracy of 1.0e-8 in 4 iterations"
            (\_ ->
                let
                    f : Float -> Float
                    f =
                        \x -> x ^ 4 - 1

                    fp : Float -> Float
                    fp =
                        \x -> 4 * x ^ 3

                    result : Result String Float
                    result =
                        NewtonIteration.iterate f fp 2.0 1.0e-8 4
                in
                case result of
                    Ok root ->
                        Expect.fail ("test failure: root = " ++ String.fromFloat root)

                    Err err ->
                        Expect.equal "too many iterations" err
            )
        , Test.test "finds a root of x^4 - 1 an accuracy of 1.0e-8 in 8 iterations"
            (\_ ->
                let
                    f : Float -> Float
                    f =
                        \x -> x ^ 4 - 1

                    fp : Float -> Float
                    fp =
                        \x -> 4 * x ^ 3

                    result : Result String Float
                    result =
                        NewtonIteration.iterate f fp 2.0 1.0e-8 8
                in
                case result of
                    Ok root ->
                        Expect.within (Expect.Absolute 1.0e-8) root 1.0

                    Err err ->
                        Expect.fail err
            )
        ]
