module Grid exposing
    ( Grid
    , fromLists
    , get
    , height
    , init
    , map
    , update
    , width
    )

import Array exposing (Array)


type Grid a
    = Grid Int Int (Array (Array a))


init : Int -> Int -> (( Int, Int ) -> a) -> Grid a
init width_ height_ initFn =
    Grid
        width_
        height_
        (Array.initialize
            height_
            (\y -> Array.initialize width_ (\x -> initFn ( x, y )))
        )


fromLists : a -> List (List a) -> Grid a
fromLists fallback lists =
    let
        arrays =
            Array.fromList (List.map Array.fromList lists)

        height_ =
            List.length lists

        width_ =
            List.head lists
                |> Maybe.map List.length
                |> Maybe.withDefault 0
    in
    init
        width_
        height_
        (\( x, y ) ->
            get ( x, y ) (Grid 0 0 arrays)
                |> Maybe.withDefault fallback
        )


get : ( Int, Int ) -> Grid a -> Maybe a
get ( x, y ) (Grid _ _ rows) =
    rows
        |> Array.get y
        |> Maybe.andThen (Array.get x)


update : ( Int, Int ) -> a -> Grid a -> Grid a
update ( x, y ) newValue (Grid width_ height_ rows) =
    rows
        |> Array.get y
        |> Maybe.map
            (\row ->
                Array.set y (Array.set x newValue row) rows
            )
        |> Maybe.map (Grid width_ height_)
        |> Maybe.withDefault (Grid width_ height_ rows)


map : (( Int, Int ) -> a -> b) -> Grid a -> List (List b)
map fn (Grid width_ height_ rows) =
    Array.toIndexedList rows
        |> List.map
            (\( y, row ) ->
                Array.toIndexedList row
                    |> List.map
                        (\( x, item ) ->
                            fn ( x, y ) item
                        )
            )


height : Grid a -> Int
height (Grid _ h _) =
    h


width : Grid a -> Int
width (Grid w _ _) =
    w
