import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debounce exposing (Debounce)
import Time
import Task
import Process


main : Program Never Model Msg
main =
  program
    { init = init
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
  { value = ""
  , debounce = Debounce.init
  , report = []
  } ! []


type Msg
  = Input String
  | Saved String
  | DebounceMsg Debounce.Msg


debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.manual -- choose `manual` strategy
  , transform = DebounceMsg
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input value ->
      let
        (debounce, cmd) =
          Debounce.push debounceConfig value model.debounce
      in
        { model
          | value = value
          , debounce = debounce
        } ! [ cmd ]

    DebounceMsg msg ->
      let
        (debounce, cmd) =
          Debounce.update
            debounceConfig
            (Debounce.takeLast save)
            msg
            model.debounce
      in
        { model | debounce = debounce } ! [ cmd ]

    -- After response (from server) arrived,
    -- you need to manually unlock the debouncer
    -- which flushes all the values accumulated at the time.
    Saved value ->
      { model
        | report = value :: model.report
      } ! [ Debounce.unlock debounceConfig ]


save : String -> Cmd Msg
save value =
  Process.sleep Time.second
    |> Task.map (always value)
    |> Task.perform Saved


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
report value =
  li [] [ text (toString value) ]
