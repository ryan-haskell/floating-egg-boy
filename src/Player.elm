module Player exposing
    ( ClimbDirection(..)
    , Command(..)
    , MoveDirection(..)
    , Player
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Map exposing (Map)



-- MODEL


type Player
    = Idle
        { direction : MoveDirection
        , location : ( Int, Int )
        }
    | Moving
        { direction : MoveDirection
        , from : ( Int, Int )
        , to : ( Int, Int )
        , percent : Float
        }
    | Climbing
        { direction : ClimbDirection
        , from : ( Int, Int )
        , to : ( Int, Int )
        , percent : Float
        }
    | Falling
        { direction : MoveDirection
        , from : ( Int, Int )
        , to : ( Int, Int )
        , percent : Float
        }


type MoveDirection
    = Left
    | Right


type ClimbDirection
    = Up
    | Down



-- INIT


init : ( Int, Int ) -> Player
init location =
    Idle
        { direction = Right
        , location = location
        }



-- UPDATE


update : Float -> Map -> Maybe Command -> Player -> Player
update time map command player =
    command
        |> Maybe.map (nextState map player)
        |> Maybe.withDefault player
        |> step time
        |> reactToEnvironment map



-- Step 1: Determine next state from user command.


type Command
    = Move MoveDirection
    | Climb ClimbDirection


nextState : Map -> Player -> Command -> Player
nextState map player command =
    case command of
        Move direction ->
            move map direction player

        Climb direction ->
            climb map direction player


move : Map -> MoveDirection -> Player -> Player
move map direction player =
    let
        canMove =
            case player of
                Idle _ ->
                    True

                Moving _ ->
                    False

                Falling _ ->
                    False

                Climbing _ ->
                    False

        nextLocation =
            case direction of
                Left ->
                    ( getX player - 1, getY player )

                Right ->
                    ( getX player + 1, getY player )
    in
    if canMove && Map.isWalkable nextLocation map then
        Moving
            { direction = direction
            , from = ( getX player, getY player )
            , to = nextLocation
            , percent = 0
            }

    else
        player


climb : Map -> ClimbDirection -> Player -> Player
climb map direction player =
    let
        canClimb =
            case player of
                Idle _ ->
                    True

                Moving _ ->
                    False

                Falling _ ->
                    False

                Climbing _ ->
                    False

        ( nextLocation, mapCanClimb ) =
            case direction of
                Up ->
                    ( ( getX player, getY player - 1 )
                    , Map.canClimbUp
                    )

                Down ->
                    ( ( getX player, getY player + 1 )
                    , Map.canClimbDown
                    )
    in
    if canClimb && mapCanClimb (getLocation player) map then
        Climbing
            { direction = direction
            , from = ( getX player, getY player )
            , to = nextLocation
            , percent = 0
            }

    else
        player



-- Step 2: Update player based on latest state


step : Float -> Player -> Player
step time player =
    case player of
        Idle _ ->
            player

        Moving info ->
            continueUntilCompleted
                time
                info
                Moving
                player

        Climbing info ->
            continueUntilCompleted
                time
                info
                Climbing
                player

        Falling info ->
            continueUntilCompleted
                time
                info
                Falling
                player


moveDirectionOf : Player -> MoveDirection
moveDirectionOf player =
    case player of
        Idle { direction } ->
            direction

        Moving { direction } ->
            direction

        Falling { direction } ->
            direction

        Climbing _ ->
            Right


continueUntilCompleted :
    Float
    -> { a | percent : Float, to : ( Int, Int ) }
    -> ({ a | percent : Float, to : ( Int, Int ) } -> Player)
    -> Player
    -> Player
continueUntilCompleted time info ctor player =
    let
        nextPercent =
            updatePosition (speedFor player) time info.percent
    in
    if nextPercent >= 1 then
        Idle
            { direction = moveDirectionOf player
            , location = info.to
            }

    else
        ctor
            { info | percent = nextPercent }


updatePosition : Float -> Float -> Float -> Float
updatePosition speed ms position =
    let
        percentPerMs =
            1 / speed
    in
    min (percentPerMs * ms + position) 1.0


speedFor : Player -> Float
speedFor player =
    case player of
        Idle _ ->
            300

        Moving _ ->
            300

        Climbing _ ->
            500

        Falling _ ->
            300



-- Step 3: React to things like falling


reactToEnvironment : Map -> Player -> Player
reactToEnvironment map player =
    if canFall player && Map.isForeground Map.Hole (getLocation player) map then
        fall player

    else
        player


canFall : Player -> Bool
canFall player =
    case player of
        Idle _ ->
            True

        Moving _ ->
            False

        Climbing _ ->
            False

        Falling _ ->
            False


fall : Player -> Player
fall player =
    Falling
        { direction = moveDirectionOf player
        , to = getLocation player |> Tuple.mapSecond ((+) 1)
        , from = getLocation player
        , percent = 0.0
        }



-- VIEW


view : Player -> Html msg
view player =
    let
        ( x, y ) =
            getAnimatedLocation player
    in
    div
        [ style "position" "absolute"
        , style "height" "50px"
        , style "width" "50px"
        , style "top" (toPixels y)
        , style "left" (toPixels x)
        , style "transform"
            (case moveDirectionOf player of
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
                [ case player of
                    Climbing _ ->
                        text ""

                    _ ->
                        div []
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
                            ]
                , case player of
                    Falling _ ->
                        div
                            [ style "background" "red"
                            , style "position" "absolute"
                            , style "top" "10px"
                            , style "height" "6px"
                            , style "width" "6px"
                            , style "right" "4px"
                            , style "border-radius" "50%"
                            ]
                            []

                    Climbing _ ->
                        text ""

                    _ ->
                        div
                            [ style "background" "black"
                            , style "position" "absolute"
                            , style "top" "10px"
                            , style "height" "2px"
                            , style "width" "10px"
                            , style "right" "2px"
                            ]
                            []
                ]
            , div
                [ class "hat"
                , style "position" "relative"
                , case player of
                    Falling _ ->
                        style "transform" "translateY(-6px)"

                    _ ->
                        style "" ""
                ]
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
                    , style "top" "8px"
                    , style "transform" "rotate(-15deg)"
                    ]
                    []
                ]
            ]
        ]


toPixels : Float -> String
toPixels =
    (*) 50 >> String.fromFloat >> (\num -> num ++ "px")



-- HELPERS


getLocation : Player -> ( Int, Int )
getLocation player =
    case player of
        Idle { location } ->
            location

        Moving { from } ->
            from

        Climbing { from } ->
            from

        Falling { to } ->
            to


getX : Player -> Int
getX =
    getLocation >> Tuple.first


getY : Player -> Int
getY =
    getLocation >> Tuple.second


getPercent : Player -> Float
getPercent player =
    case player of
        Idle _ ->
            0.0

        Moving { percent } ->
            percent

        Climbing { percent } ->
            percent

        Falling { percent } ->
            percent


getAnimatedLocation : Player -> ( Float, Float )
getAnimatedLocation player =
    case player of
        Moving { direction, to, from } ->
            case direction of
                Left ->
                    ( toFloat (Tuple.first from) - getPercent player
                    , toFloat (Tuple.second from)
                    )

                Right ->
                    ( toFloat (Tuple.first from) + getPercent player
                    , toFloat (Tuple.second from)
                    )

        Idle { location } ->
            location |> Tuple.mapBoth toFloat toFloat

        Climbing { direction, to, from } ->
            case direction of
                Up ->
                    ( toFloat (Tuple.first from)
                    , toFloat (Tuple.second from) - getPercent player
                    )

                Down ->
                    ( toFloat (Tuple.first from)
                    , toFloat (Tuple.second from) + getPercent player
                    )

        Falling { to, from } ->
            ( toFloat (Tuple.first from)
            , toFloat (Tuple.second from) + getPercent player
            )
