import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debounce as Debounce exposing (Debounce)
import Time exposing (..)
import Task exposing (..)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { value : String
  , debounce : Debounce String Msg
  , report : List String
  }


init : ( Model, Cmd Msg )
init =
  { value = ""
  , debounce =
      Debounce.init
        { strategy = Debounce.later (1 * second)
        , transform = DebounceMsg
        , send = Debounce.takeLast save
        }
  , report = []
  } ! []


save : String -> Cmd Msg
save s =
  Task.perform
    (\_ -> NoOp)
    Saved
    (Task.succeed s)


type Msg
  = NoOp
  | Input String
  | Saved String
  | DebounceMsg Debounce.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    Input s ->
      let
        (debounce, cmd) =
          Debounce.push s model.debounce
      in
        { model
        | value = s
        , debounce = debounce
        } ! [ cmd ]

    Saved s ->
      { model
      | report = s :: model.report
      } ! []

    DebounceMsg msg ->
      let
        (debounce, cmd) =
          Debounce.update msg model.debounce
      in
        { model | debounce = debounce } ! [ cmd ]


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
  li [] [text (toString s)]
