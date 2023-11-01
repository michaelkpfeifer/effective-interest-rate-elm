module ListUtilsTests exposing (findComplex, findNothing, findSimple, productTest)

import Expect
import ListUtils
import Test


findSimple : Test.Test
findSimple =
    Test.test
        "finds an item according to a given predicate"
        (\_ ->
            Expect.equal
                (Just 2)
                (ListUtils.find (\n -> n > 1) [ 0, 1, 2, 3 ])
        )


findComplex : Test.Test
findComplex =
    Test.test
        "finds an item using a somewhat more complicated predicate"
        (\_ ->
            Expect.equal
                (Just { fn = "Wayne", ln = "Coyne" })
                (ListUtils.find
                    (\r -> r.fn == "Wayne")
                    [ { fn = "Tom", ln = "Waits" }, { fn = "Wayne", ln = "Coyne" } ]
                )
        )


findNothing : Test.Test
findNothing =
    Test.test
        "returns Nothing if there is no item satisfying the predicate "
        (\_ ->
            Expect.equal
                Nothing
                (ListUtils.find (\n -> n < 0) [ 0, 1, 2, 3 ])
        )


productTest : Test.Test
productTest =
    Test.describe "ListUtils.product"
        [ Test.test "returns the cartesian product of the given lists"
            (\_ ->
                let
                    la : List Int
                    la =
                        [ 1, 2 ]

                    lb : List String
                    lb =
                        [ "a", "b" ]
                in
                Expect.equal
                    (ListUtils.product la lb)
                    [ ( 1, "a" ), ( 1, "b" ), ( 2, "a" ), ( 2, "b" ) ]
            )
        ]
