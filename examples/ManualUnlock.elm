module Main exposing (..)

import Browser
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Task exposing (..)


main : Program () Model Msg
main =
    Browser.embed
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { value : String
    , debounce : Debounce String
    , report : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { value = ""
      , debounce = Debounce.init -- Initialize the debouncer.
      , report = []
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | Input String
    | Saved String
    | DebounceMsg Debounce.Msg


{-| This defines how the debouncer should work.
Choose the strategy for your use case.
-}
debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.manual
    , transform = DebounceMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Input s ->
            let
                -- Push your values here.
                ( debounce, cmd ) =
                    Debounce.push debounceConfig s model.debounce
            in
            ( { model
                | value = s
                , debounce = debounce
              }
            , cmd
            )

        -- This is where commands are actually sent.
        -- The logic can be dependent on the current model.
        -- You can also use all the accumulated values.
        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )

        Saved s ->
            ( { model
                | report = s :: model.report
              }
              -- Manually unlock.
            , Debounce.unlock debounceConfig
            )


save : String -> Cmd Msg
save s =
    Task.perform Saved (Process.sleep 1000 |> Task.map (always s))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.value, onInput Input ] []
        , ul [] (List.map report (List.reverse model.report))
        ]


report : String -> Html msg
report s =
    li [] [ text s ]
