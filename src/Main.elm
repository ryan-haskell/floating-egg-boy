module Main exposing (main)

import Browser
import Grid exposing (Grid)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    { map : Map
    , player : Player
    }


type alias Map =
    Grid Tile


type alias Tile =
    { background : TileBackground
    , foreground : TileForeground
    }


type TileBackground
    = Wall
    | Door
    | Sky


type TileForeground
    = Grass
    | Hole
    | Rock


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    }


type Direction
    = Left
    | Right



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        map
        initPlayer
    , Cmd.none
    )


map : Grid Tile
map =
    Grid.fromLists
        (Tile Wall Hole)
        [ [ Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Sky Hole ]
        , [ Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Sky Hole ]
        , [ Tile Sky Hole, Tile Sky Hole, Tile Sky Hole, Tile Wall Hole, Tile Sky Hole ]
        , [ Tile Sky Grass, Tile Wall Grass, Tile Wall Grass, Tile Wall Grass, Tile Sky Grass ]
        ]


initPlayer : Player
initPlayer =
    { x = 0
    , y = Grid.height map - 1
    , direction = Right
    }



-- UPDATE


type Msg
    = Move Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            let
                player =
                    model.player

                newX =
                    case direction of
                        Left ->
                            max 0 (model.player.x - 1)

                        Right ->
                            min (Grid.width map - 1) (model.player.x + 1)
            in
            ( { model
                | player =
                    { player | x = newX, direction = direction }
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Move Left) ] [ text "left" ]
        , button [ onClick (Move Right), style "margin-left" "8px" ] [ text "right" ]
        , div [ style "display" "flex" ] [ viewGame model ]
        ]


empty =
    text ""


viewGame : Model -> Html msg
viewGame model =
    div
        [ class "grid"
        , style "position" "relative"
        , style "margin-top" "8px"
        ]
        ((Grid.map (viewTile model.player) model.map
            |> List.map
                (div
                    [ class "grid__row"
                    , style "display" "flex"
                    ]
                )
         )
            ++ [ viewPlayer model.player ]
        )


viewTile : Player -> ( Int, Int ) -> Tile -> Html msg
viewTile player ( x, y ) tile =
    div
        [ style "width" "50px"
        , style "height" "50px"
        , style "position" "relative"
        ]
        [ viewBackground tile.background
        , viewForeground tile.foreground
        ]


viewBackground : TileBackground -> Html msg
viewBackground background =
    let
        color =
            case background of
                Wall ->
                    "#aaa"

                Door ->
                    "brown"

                Sky ->
                    "#3399ff"
    in
    div
        [ style "background" color
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        ]
        []


viewForeground : TileForeground -> Html msg
viewForeground foreground =
    let
        color =
            case foreground of
                Grass ->
                    "#389838"

                Rock ->
                    "gray"

                Hole ->
                    "#00000000"
    in
    div
        [ style "background" color
        , style "position" "absolute"
        , style "height" "20%"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        ]
        []


viewPlayer : Player -> Html msg
viewPlayer player =
    div
        [ style "position" "absolute"
        , style "height" "50px"
        , style "width" "50px"
        , style "top" (toPixels player.y)
        , style "left" (toPixels player.x)
        , style "transition"
            "top 300ms ease-out, left 300ms ease-out"
        , style "transform"
            (case player.direction of
                Left ->
                    "scaleX(-1)"

                Right ->
                    ""
            )
        ]
        [ div
            [ style "position" "absolute"
            , style "height" "100%"
            , style "width" "100%"
            , style "animation" "floaty 1.5s infinite"
            ]
            [ div
                [ style "background" "white"
                , style "position" "absolute"
                , style "height" "50%"
                , style "left" "30%"
                , style "right" "30%"
                , style "bottom" "30%"
                , style "border-radius" "50%"
                ]
                [ div
                    [ style "background" "black"
                    , style "position" "absolute"
                    , style "top" "6px"
                    , style "height" "2px"
                    , style "width" "2px"
                    , style "right" "12px"
                    ]
                    []
                , div
                    [ style "background" "black"
                    , style "position" "absolute"
                    , style "top" "6px"
                    , style "height" "2px"
                    , style "width" "2px"
                    , style "right" "2px"
                    ]
                    []
                , div
                    [ style "background" "black"
                    , style "position" "absolute"
                    , style "top" "10px"
                    , style "height" "2px"
                    , style "width" "10px"
                    , style "right" "2px"
                    ]
                    []
                ]
            , div []
                [ div
                    [ style "background" "#9f5030"
                    , style "position" "absolute"
                    , style "height" "12px"
                    , style "left" "30%"
                    , style "right" "45%"
                    , style "top" "5%"
                    , style "transform" "rotate(-15deg)"
                    ]
                    []
                , div
                    [ style "background" "#9f5030"
                    , style "position" "absolute"
                    , style "height" "4px"
                    , style "left" "25%"
                    , style "right" "35%"
                    , style "top" "20%"
                    , style "transform" "rotate(-15deg)"
                    ]
                    []
                ]
            ]
        ]


toPixels : Int -> String
toPixels =
    (*) 50 >> String.fromInt >> (\num -> num ++ "px")
