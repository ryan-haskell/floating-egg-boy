module Map exposing
    ( Background(..)
    , Foreground(..)
    , Map
    , canClimbDown
    , canClimbUp
    , get
    , height
    , init
    , isBackground
    , isForeground
    , isWalkable
    , view
    , width
    )

import Grid exposing (Grid)
import Html exposing (..)
import Html.Attributes exposing (class, style)


type Map
    = Map (Grid Tile)


type alias Tile =
    { background : Background
    , foreground : Foreground
    }


type Background
    = Wall
    | Ladder
    | Door
    | Sky


type Foreground
    = Grass
    | LadderTop
    | Hole
    | Rock



-- Data access stuff


width : Map -> Int
width (Map grid) =
    Grid.width grid


height : Map -> Int
height (Map grid) =
    Grid.height grid


get : ( Int, Int ) -> Map -> Maybe Tile
get location (Map grid) =
    Grid.get location grid


inBounds : ( Int, Int ) -> Map -> Bool
inBounds ( x, y ) (Map grid) =
    (x >= 0)
        && (x < Grid.width grid)
        && (y >= 0)
        && (y < Grid.height grid)



-- walking


isWalkable : ( Int, Int ) -> Map -> Bool
isWalkable location (Map grid) =
    inBounds location (Map grid)
        && (Grid.get location grid
                |> Maybe.map .foreground
                |> Maybe.map isForegroundWalkable
                |> Maybe.withDefault False
           )


isForegroundWalkable : Foreground -> Bool
isForegroundWalkable foreground =
    case foreground of
        Grass ->
            True

        LadderTop ->
            True

        Hole ->
            True

        Rock ->
            False



-- climbing


canClimbUp : ( Int, Int ) -> Map -> Bool
canClimbUp location (Map grid) =
    inBounds location (Map grid)
        && (Grid.get location grid
                |> Maybe.map .background
                |> Maybe.map isBackgroundClimbable
                |> Maybe.withDefault False
           )


canClimbDown : ( Int, Int ) -> Map -> Bool
canClimbDown location (Map grid) =
    inBounds location (Map grid)
        && (Grid.get location grid
                |> Maybe.map .foreground
                |> Maybe.map isForegroundClimbable
                |> Maybe.withDefault False
           )


isBackgroundClimbable : Background -> Bool
isBackgroundClimbable background =
    case background of
        Ladder ->
            True

        Door ->
            False

        Wall ->
            False

        Sky ->
            False


isForegroundClimbable : Foreground -> Bool
isForegroundClimbable foreground =
    case foreground of
        Grass ->
            False

        LadderTop ->
            True

        Hole ->
            False

        Rock ->
            False


isBackground : Background -> ( Int, Int ) -> Map -> Bool
isBackground background ( x, y ) (Map grid) =
    Grid.get ( x, y ) grid
        |> Maybe.map .background
        |> Maybe.map ((==) background)
        |> Maybe.withDefault False


isForeground : Foreground -> ( Int, Int ) -> Map -> Bool
isForeground foreground ( x, y ) (Map grid) =
    Grid.get ( x, y ) grid
        |> Maybe.map .foreground
        |> Maybe.map ((==) foreground)
        |> Maybe.withDefault False



-- INIT


init : Map
init =
    Map <|
        Grid.fromLists
            (Tile Wall Hole)
            [ [ Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Sky Hole ]
            , [ Tile Sky Hole, Tile Sky Hole, Tile Sky Grass, Tile Sky LadderTop, Tile Sky Hole ]
            , [ Tile Sky Hole, Tile Sky Grass, Tile Wall LadderTop, Tile Ladder Grass, Tile Sky Hole ]
            , [ Tile Sky Grass, Tile Wall Grass, Tile Ladder Grass, Tile Wall Grass, Tile Sky Grass ]
            ]



-- VIEW


view : Map -> Html msg
view (Map grid) =
    div [ class "map" ]
        (List.map
            (div [ class "map__row" ])
            (Grid.map (always viewTile) grid)
        )


viewTile : Tile -> Html msg
viewTile tile =
    div [ class "map__tile" ]
        [ viewBackground tile.background
        , viewForeground tile.foreground
        ]


viewBackground : Background -> Html msg
viewBackground background =
    case background of
        Sky ->
            div [ class "bg bg--sky" ] []

        Wall ->
            div [ class "bg bg--wall" ] []

        Door ->
            div [ class "bg bg--door" ] []

        Ladder ->
            div [ class "bg bg--ladder" ] []


viewForeground : Foreground -> Html msg
viewForeground foreground =
    case foreground of
        Grass ->
            div [ class "fg fg--grass" ] []

        LadderTop ->
            div [ class "fg fg--laddertop" ] []

        Hole ->
            div [ class "fg fg--hole" ] []

        Rock ->
            div [ class "fg fg--rock" ] []
