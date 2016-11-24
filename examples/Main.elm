import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debounce exposing (Debounce)
import Time
import Task


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
  , debounce : Debounce String -- Deboucer that processes String values
  , report : List String
  }


init : ( Model, Cmd Msg )
init =
  { value = ""
  -- Initialize the debouncer.
  , debounce = Debounce.init
  , report = []
  } ! []


type Msg
  = Input String
  | DebounceMsg Debounce.Msg
  | Saved String


-- This defines how the debouncer should work.
-- Choose the strategy for your use case.
debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.later (1 * Time.second) -- 1s after getting stable
  , transform = DebounceMsg                     -- pass Msg wrapper
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input value ->
      let
        -- Push your values. They are not immediately processed.
        (debounce, cmd) =
          Debounce.push debounceConfig value model.debounce
      in
        { model
          | value = value
          , debounce = debounce
        } ! [ cmd ]

    -- This is where debounced values are processed and sent as Cmd.
    -- All the accumulated values are available here.
    -- You can choose how to reduce them.
    DebounceMsg msg ->
      let
        (debounce, cmd) =
          Debounce.update
            debounceConfig
            (Debounce.takeLast save) -- save the last value
            msg
            model.debounce
      in
        { model | debounce = debounce } ! [ cmd ]

    Saved value ->
      { model
        | report = value :: model.report
      } ! []


save : String -> Cmd Msg
save value =
  Task.succeed value
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
