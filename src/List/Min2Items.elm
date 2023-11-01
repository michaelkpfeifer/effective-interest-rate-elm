module List.Min2Items exposing (Min2Items(..), first, foldl, map, minimumWith, rest, second, toList)


type Min2Items a
    = Min2Items a a (List a)


first : Min2Items a -> a
first (Min2Items fst _ _) =
    fst


second : Min2Items a -> a
second (Min2Items _ snd _) =
    snd


rest : Min2Items a -> List a
rest (Min2Items _ _ rst) =
    rst


toList : Min2Items a -> List a
toList (Min2Items fst snd rst) =
    fst :: snd :: rst


map : (a -> b) -> Min2Items a -> Min2Items b
map fn (Min2Items fst snd rst) =
    Min2Items (fn fst) (fn snd) (List.map fn rst)


foldl : (a -> b -> b) -> b -> Min2Items a -> b
foldl fn acc items =
    List.foldl fn acc (toList items)


minimumWith : (a -> a -> Order) -> Min2Items a -> a
minimumWith cmpFn (Min2Items fst snd rst) =
    List.foldl
        (\x y ->
            case cmpFn x y of
                LT ->
                    x

                _ ->
                    y
        )
        fst
        (snd :: rst)
