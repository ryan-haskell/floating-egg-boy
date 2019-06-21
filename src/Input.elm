module Input exposing
    ( Gamepad
    , Input(..)
    , Key(..)
    , KeyAction(..)
    , KeyEvent
    , gamepadUpdate
    , init
    , keyboardUpdate
    )


type Input
    = UsingKeyboard (List Key)
    | UsingGamepad Gamepad



-- Keyboard controls


type alias KeyEvent =
    { direction : KeyAction
    , key : Key
    }


type KeyAction
    = Pressed
    | Released


type Key
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight



-- Gamepad controls


type alias Gamepad =
    { x : Float
    , y : Float
    }



-- INIT


init : Input
init =
    UsingKeyboard []



-- UPDATE


keyboardUpdate : KeyEvent -> Input -> Input
keyboardUpdate action input =
    let
        currentKeys : List Key
        currentKeys =
            case input of
                UsingKeyboard keys ->
                    keys

                UsingGamepad _ ->
                    []

        pressKey key keys =
            if List.member key keys then
                keys

            else
                key :: keys

        releaseKey key keys =
            List.filter ((/=) key) keys
    in
    currentKeys
        |> (case action.direction of
                Pressed ->
                    pressKey action.key

                Released ->
                    releaseKey action.key
           )
        |> UsingKeyboard


gamepadUpdate : Gamepad -> Input
gamepadUpdate gamepad =
    UsingGamepad gamepad
