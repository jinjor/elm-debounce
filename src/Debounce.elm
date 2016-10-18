module Debounce exposing
  ( Debounce, Msg
  , Config, init
  , Strategy, soon, later
  , Send, takeLast, takeAll
  , update, push
  )

{-| The Debouncer. See the full example [here](https://github.com/jinjor/elm-debounce/blob/master/examples/Main.elm).

* This module works with the Elm Architecture.
* You can choose the strategy and define how commands are sent.

# Types
@docs Debounce, Msg

# Initialize
@docs Config, init

# Strategies
@docs Strategy, soon, later

# Sending Commands
@docs Send, takeLast, takeAll

# Update
@docs update, push

-}

import Time exposing (..)
import Task exposing (..)
import Process


{-| The state of the debouncer.

It is parameterized with the value type `a` and the user's message tyle `msg`.
-}
type Debounce a msg =
  Debounce
    { config : Config msg
    , input : List a
    , locked : Bool
    }


{-| Config contains the debouncing strategy and the message transformation.
-}
type alias Config msg =
  { strategy : Strategy
  , transform : Msg -> msg
  }


{-| Strategy defines the timing when commands are sent.
-}
type Strategy
  = Soon Time
  | Later Time


{-| Send command as soon as possible, with given rate limit. (a.k.a. Throttle)
-}
soon : Time -> Strategy
soon = Soon


{-| Send command after becomming stable, with given rate limit. (a.k.a. Debounce)
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
init : Config msg -> Debounce a msg
init config =
  Debounce
    { config = config
    , input = []
    , locked = False
    }


{-| The messages that are used internally.
-}
type Msg
  = NoOp
  | Delay Int


{-| This is the component's update function following the Elm Architecture.

e.g. Saving the last value.
```
(debounce, cmd) =
  Debounce.update
    (Debounce.takeLast save) -- save : value -> Cmd Msg
    msg
    model.debounce
```
The sending logic can depend on the current model. If you want to stop sending, return `Cmd.none`.

-}
update : Send a msg -> Msg -> Debounce a msg -> (Debounce a msg, Cmd msg)
update send msg (Debounce d) =
  case msg of
    NoOp ->
      (Debounce d) ! []

    Delay len ->
      case d.config.strategy of
        Soon delay ->
          case d.input of
            head :: tail ->
              let
                (input, sendCmd) =
                  send head tail

                selfCmd =
                  Cmd.map d.config.transform
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

{-| Push a value into the debouncer.
-}
push : a -> Debounce a msg -> (Debounce a msg, Cmd msg)
push a (Debounce d) =
  case d.config.strategy of
    Soon delay ->
      if d.locked then
        (Debounce { d | input = a :: d.input }, Cmd.none)
      else
        ( Debounce { d | input = a :: d.input }
        , Cmd.map d.config.transform
          (delayCmd 0 (List.length d.input + 1))
        )

    Later delay ->
      let
        debounce =
          Debounce { d | input = a :: d.input }

        cmd =
          Cmd.map d.config.transform
            (delayCmd delay (List.length d.input + 1))
      in
        (debounce, cmd)


delayCmd : Time -> Int -> Cmd Msg
delayCmd delay inputCount =
  Task.perform (\_ -> NoOp) Delay
    ( Process.sleep delay `andThen` \_ ->
      Task.succeed inputCount
    )
