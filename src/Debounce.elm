module Debounce exposing
  ( Debounce, Msg
  , Config, init
  , Strategy, soon, soonAfter, later, manual
  , Send, takeLast, takeAll
  , update, push, unlock
  )

{-| The Debouncer. See the full example [here](https://github.com/jinjor/elm-debounce/blob/master/examples/Main.elm).

* This module works with the Elm Architecture.
* You can choose the strategy and define how commands are sent.

# Types
@docs Debounce, Msg

# Initialize
@docs Config, init

# Strategies
@docs Strategy, soon, soonAfter, later, manual

# Sending Commands
@docs Send, takeLast, takeAll

# Update
@docs update, push, unlock

-}

import Time exposing (..)
import Task exposing (..)
import Process


{-| The state of the debouncer.

It is parameterized with the value type `a`.
-}
type Debounce a =
  Debounce
    { input : List a
    , locked : Bool
    }


{-| Config contains the debouncing strategy and the message transformation.

This config should be constant and shared between `update` function and `push` function.
-}
type alias Config msg =
  { strategy : Strategy
  , transform : Msg -> msg
  }


{-| Strategy defines the timing when commands are sent.
-}
type Strategy
  = Manual
  | Soon Time Time
  | Later Time


{-| Send commands when it is manually unlocked. See `unlock`.

Typically, `unlock` is called after previous response comes back.
-}
manual : Strategy
manual = Manual


{-| Send command as soon as it gets ready, with given rate limit. (a.k.a. Throttle)

Note: The first command will be sent immidiately.
-}
soon : Time -> Strategy
soon = Soon 0


{-| Similar to `soon`, but send first command after offset time.
-}
soonAfter : Time -> Time -> Strategy
soonAfter = Soon


{-| Send command after becomming stable, with given delay time. (a.k.a. Debounce)
-}
later : Time -> Strategy
later = Later


{-| This function consumes values and send a command.

If you want to postpone sending, return the values back to keep them.
-}
type alias Send a msg =
  a -> List a -> (List a, Cmd msg)


{-| Send a command using the latest value.
-}
takeLast : (a -> Cmd msg) -> Send a msg
takeLast send head tail =
  ([], send head)


{-| Send a command using all the accumulated values.
-}
takeAll : (a -> List a -> Cmd msg) -> Send a msg
takeAll send head tail =
  ([], send head tail)


{-| Initialize the debouncer. Call this from your `init` function.
-}
init : Debounce a
init =
  Debounce
    { input = []
    , locked = False
    }


{-| The messages that are used internally.
-}
type Msg
  = NoOp
  | UnlockAndSend
  | Delay Int


{-| This is the component's update function following the Elm Architecture.

e.g. Saving the last value.
```
(debounce, cmd) =
  Debounce.update
    { strategy = Debounce.later (1 * second)
    , transform = DebounceMsg
    }
    (Debounce.takeLast save) -- save : value -> Cmd Msg
    msg
    model.debounce
```
The config should be constant and shared with `push` function.

The sending logic can depend on the current model. If you want to stop sending, return `Cmd.none`.

-}
update : Config msg -> Send a msg -> Msg -> Debounce a -> (Debounce a, Cmd msg)
update config send msg (Debounce d) =
  case msg of
    NoOp ->
      (Debounce d) ! []

    UnlockAndSend ->
      case d.input of
        head :: tail ->
          let
            (input, sendCmd) =
              send head tail
          in
            Debounce
              { d
              | input = input
              , locked = True
              } ! [ sendCmd ]

        _ ->
          Debounce { d | locked = False } ! []

    Delay len ->
      case config.strategy of
        Manual ->
          case d.input of
            head :: tail ->
              let
                (input, sendCmd) =
                  send head tail
              in
                Debounce
                  { d
                  | input = input
                  , locked = True
                  } ! [ sendCmd ]

            _ ->
              Debounce d ! []

        Soon _ delay ->
          case d.input of
            head :: tail ->
              let
                (input, sendCmd) =
                  send head tail

                selfCmd =
                  Cmd.map config.transform
                    (delayCmd delay (List.length d.input + 1))
              in
                Debounce
                  { d
                  | input = input
                  , locked = True
                  } ! [ sendCmd, selfCmd ]

            _ ->
              Debounce { d | locked = False } ! []

        Later _ ->
          case (List.length d.input <= len, d.input) of
            (True, head :: tail) ->
              let
                (input, cmd) =
                  send head tail
              in
                ( Debounce { d | input = input }
                , cmd
                )

            _ ->
              (Debounce d) ! []


{-| Manually unlock. This works for `manual` Strategy.
-}
unlock : Config msg -> Cmd msg
unlock config =
  Cmd.map config.transform <|
    Task.perform
      (\_ -> NoOp)
      identity
      (Task.succeed UnlockAndSend)



{-| Push a value into the debouncer.
-}
push : Config msg -> a -> Debounce a -> (Debounce a, Cmd msg)
push config a (Debounce d) =
  case config.strategy of
    Manual ->
      ( Debounce { d | input = a :: d.input }
      , if d.locked then
          Cmd.none
        else
          Cmd.map
            config.transform
            (delayCmd 0 (List.length d.input + 1))
      )

    Soon offset delay ->
      ( Debounce { d | input = a :: d.input }
      , if d.locked then
          Cmd.none
        else
          Cmd.map
            config.transform
            (delayCmd offset (List.length d.input + 1))
      )

    Later delay ->
      ( Debounce { d | input = a :: d.input }
      , Cmd.map
          config.transform
          (delayCmd delay (List.length d.input + 1))
      )


delayCmd : Time -> Int -> Cmd Msg
delayCmd delay inputCount =
  Task.perform (\_ -> NoOp) Delay
    ( Process.sleep delay `andThen` \_ ->
      Task.succeed inputCount
    )
