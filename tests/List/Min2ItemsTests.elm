module List.Min2ItemsTests exposing (firstTest, foldlTest, mapTest, minimumWithTest, restTest, secondTest, toListTest)

import Expect
import List.Min2Items
import Test


firstTest : Test.Test
firstTest =
    Test.describe "List.Min2Items.first"
        [ Test.test "it returns the first item"
            (\_ ->
                let
                    items : List.Min2Items.Min2Items Int
                    items =
                        List.Min2Items.Min2Items 1 2 [ 3, 4 ]
                in
                Expect.equal (List.Min2Items.first items) 1
            )
        ]


secondTest : Test.Test
secondTest =
    Test.describe "List.Min2Items.second"
        [ Test.test "it returns the second item"
            (\_ ->
                let
                    items : List.Min2Items.Min2Items Int
                    items =
                        List.Min2Items.Min2Items 1 2 [ 3, 4 ]
                in
                Expect.equal (List.Min2Items.second items) 2
            )
        ]


restTest : Test.Test
restTest =
    Test.describe "List.Min2Items.rest"
        [ Test.test "it returns the rest of the items"
            (\_ ->
                let
                    items : List.Min2Items.Min2Items Int
                    items =
                        List.Min2Items.Min2Items 1 2 [ 3, 4 ]
                in
                Expect.equal (List.Min2Items.rest items) [ 3, 4 ]
            )
        ]


toListTest : Test.Test
toListTest =
    Test.describe "List.Min2Items.toList"
        [ Test.test "it works with a short list of two items"
            (\_ ->
                Expect.equal
                    (List.Min2Items.toList (List.Min2Items.Min2Items 1 2 []))
                    [ 1, 2 ]
            )
        , Test.test "it works with a longer list"
            (\_ ->
                Expect.equal
                    (List.Min2Items.toList (List.Min2Items.Min2Items 1 2 [ 3, 4, 5 ]))
                    [ 1, 2, 3, 4, 5 ]
            )
        ]


mapTest : Test.Test
mapTest =
    Test.describe "List.Min2Items.map"
        [ Test.test "it maps the identity over two entries"
            (\_ ->
                let
                    items : List.Min2Items.Min2Items Int
                    items =
                        List.Min2Items.Min2Items 1 2 []
                in
                Expect.equal (List.Min2Items.map identity items) items
            )
        , Test.test "it computes the squares of some integers"
            (\_ ->
                let
                    items : List.Min2Items.Min2Items Int
                    items =
                        List.Min2Items.Min2Items 5 4 [ 3, 2, 1 ]
                in
                Expect.equal
                    (List.Min2Items.Min2Items 25 16 [ 9, 4, 1 ])
                    (List.Min2Items.map (\n -> n * n) items)
            )
        ]


foldlTest : Test.Test
foldlTest =
    Test.describe "List.Min2Items.foldl"
        [ Test.test "it folds a simple case by adding all items"
            (\_ ->
                Expect.equal
                    (List.Min2Items.foldl
                        (+)
                        0
                        (List.Min2Items.Min2Items 1 2 [ 3, 4, 5 ])
                    )
                    15
            )
        ]


minimumWithTest : Test.Test
minimumWithTest =
    Test.describe "List.Min2Items.minimumWith"
        [ Test.test "it works in a very simple case"
            (\_ ->
                let
                    items : List.Min2Items.Min2Items Int
                    items =
                        List.Min2Items.Min2Items 2 1 []
                in
                Expect.equal (List.Min2Items.minimumWith compare items) 1
            )
        , Test.test "it works with records"
            (\_ ->
                let
                    a : { char : String, code : Int }
                    a =
                        { char = "a", code = 97 }

                    b : { char : String, code : Int }
                    b =
                        { char = "b", code = 98 }

                    c : { char : String, code : Int }
                    c =
                        { char = "c", code = 99 }

                    d : { char : String, code : Int }
                    d =
                        { char = "d", code = 100 }

                    items : List.Min2Items.Min2Items { char : String, code : Int }
                    items =
                        List.Min2Items.Min2Items b d [ a, c ]

                    cmpFn : { char : String, code : Int } -> { char : String, code : Int } -> Order
                    cmpFn =
                        \x y -> compare x.code y.code
                in
                Expect.equal (List.Min2Items.minimumWith cmpFn items) a
            )
        ]
