module Rag.Index exposing (Index, Vector, build, isEmpty, query)


type alias Vector =
    List Float


type Index a
    = Index (List ( Vector, a ))


build : List ( Vector, a ) -> Index a
build =
    Index


isEmpty : Index a -> Bool
isEmpty (Index xs) =
    List.isEmpty xs


query : Vector -> Int -> Index a -> List a
query q k (Index entries) =
    entries
        |> List.map (\( v, payload ) -> ( dot q v, payload ))
        |> List.sortBy (Tuple.first >> negate)
        |> List.take k
        |> List.map Tuple.second


dot : Vector -> Vector -> Float
dot a b =
    -- Vectors are unit-normalized by Transformers.js, so cosine reduces to dot.
    List.map2 (*) a b |> List.sum
