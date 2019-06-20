module Main exposing (main)

import Browser
import Browser.Events as Events
import Grid exposing (Grid)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as D exposing (Decoder)
import Process
import Task



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
    | Ladder
    | Door
    | Sky


type TileForeground
    = Grass
    | LadderTop
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
    | Up
    | Down



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
        , [ Tile Sky Hole, Tile Sky Hole, Tile Sky Grass, Tile Sky Grass, Tile Sky Hole ]
        , [ Tile Sky Hole, Tile Sky Grass, Tile Wall LadderTop, Tile Wall Grass, Tile Sky Hole ]
        , [ Tile Sky Grass, Tile Wall Grass, Tile Ladder Grass, Tile Wall Grass, Tile Sky Grass ]
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
    | FallAndStuff


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            let
                player =
                    model.player

                ( newX, newY ) =
                    case direction of
                        Left ->
                            ( max 0 (player.x - 1), player.y )

                        Right ->
                            ( min (Grid.width map - 1) (player.x + 1), player.y )

                        Up ->
                            ( player.x
                            , if isPlayerOnBackground model Ladder then
                                max 0 (model.player.y - 1)

                              else
                                player.y
                            )

                        Down ->
                            ( player.x
                            , if isPlayerOnForeground model LadderTop then
                                min (Grid.height map - 1) (model.player.y + 1)

                              else
                                player.y
                            )
            in
            ( { model
                | player =
                    { player
                        | x = newX
                        , y = newY
                        , direction = direction
                    }
              }
            , delay 300 FallAndStuff
            )

        FallAndStuff ->
            let
                player =
                    model.player

                isPlayerOnGround =
                    List.any (isPlayerOnForeground model)
                        [ Grass
                        , LadderTop
                        , Rock
                        ]
            in
            ( if isPlayerOnGround then
                model

              else
                { model | player = { player | y = max (Grid.height map - 1) (player.y + 1) } }
            , Cmd.none
            )


delay : Float -> Msg -> Cmd Msg
delay ms msg =
    Process.sleep ms
        |> Task.perform (always msg)


isPlayerOnBackground : Model -> TileBackground -> Bool
isPlayerOnBackground model background =
    Grid.get ( model.player.x, model.player.y ) model.map
        |> Maybe.map .background
        |> Maybe.map ((==) background)
        |> Maybe.withDefault False


isPlayerOnForeground : Model -> TileForeground -> Bool
isPlayerOnForeground model foreground =
    Grid.get ( model.player.x, model.player.y ) model.map
        |> Maybe.map .foreground
        |> Maybe.map ((==) foreground)
        |> Maybe.withDefault False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown keyDownDecoder
        ]


keyDownDecoder : Decoder Msg
keyDownDecoder =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        D.succeed (Move Left)

                    "ArrowRight" ->
                        D.succeed (Move Right)

                    "ArrowUp" ->
                        D.succeed (Move Up)

                    "ArrowDown" ->
                        D.succeed (Move Down)

                    _ ->
                        D.fail "unrecognized key"
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "mr. floating egg boy" ]
        , p [ style "margin-top" "-1.5rem" ] [ text "arrow keys to move left and right!" ]
        , div [ style "display" "flex" ] [ viewGame model ]
        , p [] [ text "Also, that gold thing is a ladder! Hit up to climb it!" ]
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

                Ladder ->
                    "gold"

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

                LadderTop ->
                    "#286838"

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

                Up ->
                    ""

                Down ->
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
