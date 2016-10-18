module Debounce exposing
  ( Debounce
  , Config
  , Strategy, soon, later
  , Send, takeLast, takeAll
  , init
  , Msg
  , update, push
  )


import Time exposing (..)
import Task exposing (..)
import Process


type Debounce a msg =
  Debounce
    { config : Config msg
    , input : List a
    , locked : Bool
    }


type alias Config msg =
  { strategy : Strategy
  , transform : Msg -> msg
  }


type Strategy
  = Soon Time
  | Later Time


soon : Time -> Strategy
soon = Soon


later : Time -> Strategy
later = Later


type alias Send a msg =
  a -> List a -> (List a, Cmd msg)


takeLast : (a -> Cmd msg) -> Send a msg
takeLast send head tail =
  ([], send head)


takeAll : (a -> List a -> Cmd msg) -> Send a msg
takeAll send head tail =
  ([], send head tail)


-- consume
-- (Is there any case where "send" requires partial input?)


init : Config msg -> Debounce a msg
init config =
  Debounce
    { config = config
    , input = []
    , locked = False
    }


type Msg
  = NoOp
  | Delay Int


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
