module ListUtils exposing (find, product)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                find predicate xs


product : List a -> List b -> List ( a, b )
product la lb =
    List.map (\va -> List.map (\vb -> ( va, vb )) lb) la
        |> List.concat
