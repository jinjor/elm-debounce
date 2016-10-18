import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debounce exposing (Debounce)
import Time exposing (..)
import Task exposing (..)


main : Program Never
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
  -- Initialize the debouncer.
  -- Choose the strategy for your use case.
  , debounce =
      Debounce.init
        { strategy = Debounce.later (1 * second)
        , transform = DebounceMsg
        }
  , report = []
  } ! []


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
        -- Push your values here.
        (debounce, cmd) =
          Debounce.push s model.debounce
      in
        { model
        | value = s
        , debounce = debounce
        } ! [ cmd ]

    -- This is where commands are actually sent.
    -- The logic can be dependent on the current model.
    -- You can also use all the accumulated values.
    DebounceMsg msg ->
      let
        (debounce, cmd) =
          Debounce.update
            (Debounce.takeLast save)
            msg
            model.debounce
      in
        { model | debounce = debounce } ! [ cmd ]

    Saved s ->
      { model
      | report = s :: model.report
      } ! []


save : String -> Cmd Msg
save s =
  Task.perform
    (\_ -> NoOp)
    Saved
    (Task.succeed s)


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
  li [] [ text (toString s) ]
