module Main exposing (Flags, Model, Msg(..), command, commandFromGamepad, commandFromKeys, init, keyDecoder, main, subscriptions, update, view)

import Browser
import Browser.Events as Events
import Grid exposing (Grid)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Input exposing (Input)
import Json.Decode as D exposing (Decoder)
import Map exposing (Map)
import Player exposing (ClimbDirection(..), Command(..), MoveDirection(..), Player)
import Process
import Set exposing (Set)
import Task



-- MAIN


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { map : Map
    , player : Player
    , input : Input
    , isPaused : Bool
    , command : Maybe Command
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        map =
            Map.init

        player =
            Player.init ( 0, Map.height map - 1 )

        input =
            Input.init
    in
    ( Model map player input False Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyboardEvent Input.KeyEvent
    | TogglePlayPause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        TogglePlayPause ->
            { model | isPaused = not model.isPaused }

        KeyboardEvent event ->
            let
                input =
                    Input.keyboardUpdate
                        event
                        model.input
            in
            { model
                | input = input
                , command =
                    command input
            }

        OnAnimationFrame time ->
            { model
                | player =
                    Player.update
                        time
                        model.map
                        (command model.input)
                        model.player
            }
    , Cmd.none
    )



-- Converting input into commands


command : Input -> Maybe Command
command input =
    Debug.log "Command" <|
        case input of
            Input.UsingKeyboard keys ->
                commandFromKeys keys

            Input.UsingGamepad gamepad ->
                commandFromGamepad gamepad


commandFromKeys : List Input.Key -> Maybe Command
commandFromKeys keys =
    [ ( Input.ArrowRight, Move Right )
    , ( Input.ArrowLeft, Move Left )
    , ( Input.ArrowUp, Climb Up )
    , ( Input.ArrowDown, Climb Down )
    ]
        |> List.filterMap
            (\( key, command_ ) ->
                if List.member key keys then
                    Just command_

                else
                    Nothing
            )
        |> List.head


commandFromGamepad : Input.Gamepad -> Maybe Player.Command
commandFromGamepad { x, y } =
    let
        threshold =
            0.25
    in
    if x > threshold then
        Just (Move Right)

    else if x < negate threshold then
        Just (Move Left)

    else if y < negate threshold then
        Just (Climb Up)

    else if y > threshold then
        Just (Climb Down)

    else
        Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Events.onKeyDown (keyDecoder Input.Pressed)
        , Events.onKeyUp (keyDecoder Input.Released)
        ]
            ++ (if model.isPaused then
                    []

                else
                    [ Events.onAnimationFrameDelta OnAnimationFrame ]
               )


keyDecoder : Input.KeyAction -> Decoder Msg
keyDecoder action =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        D.succeed <|
                            KeyboardEvent
                                (Input.KeyEvent action Input.ArrowLeft)

                    "ArrowRight" ->
                        D.succeed <|
                            KeyboardEvent
                                (Input.KeyEvent action Input.ArrowRight)

                    "ArrowUp" ->
                        D.succeed <|
                            KeyboardEvent
                                (Input.KeyEvent action Input.ArrowUp)

                    "ArrowDown" ->
                        D.succeed <|
                            KeyboardEvent
                                (Input.KeyEvent action Input.ArrowDown)

                    " " ->
                        if action == Input.Released then
                            D.succeed TogglePlayPause

                        else
                            D.fail "Do nothing"

                    _ ->
                        D.fail "unrecognized key"
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "mr. floating egg boy" ]
        , p [] [ text "a game about true love" ]
        , div [ class "game" ]
            [ Map.view model.map
            , Player.view model.player
            ]
        , p [] [ text "arrow keys to move, and those gold things are ladders." ]
        ]
