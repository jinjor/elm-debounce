module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Debounce exposing (Debounce)
import Time exposing (Time)
import Task exposing (Task)


main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


{-| The model contains a report (a `List`) of each time the debouncer settled on a value.
It also contains a string debouncer (`Debounce String`) which is the object manipulated.
by the `Debounce` module.
-}
type alias Model =
    { debounce : Debounce String
    , report : List String
    }


{-| Initialize the debouncer and the report.
-}
init : ( Model, Cmd Msg )
init =
    { debounce = Debounce.init
    , report = []
    }
        ! []


{-| The debouncing mechanism requires the addition of two messages:
* Saved String: for when the debounced string value settled.
* DebounceMsg Debounce.Msg: for the module internal messages.
-}
type Msg
    = Input String
    | Saved String
    | DebounceMsg Debounce.Msg


{-| This defines how the debouncer should work.
Choose the strategy for your use case.
Thes most common stategies are:
* For debouncing: later duration
* For throttle: soon duration
* For manual control: manual
-}
debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (1 * Time.second)
    , transform = DebounceMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Previously, this was where the Input message was handled.
        -- Now the Input message is redirected (using Debounce.push)
        -- to the Debounce module that will handle it.
        Input str ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig str model.debounce
            in
                { model | debounce = debounce } ! [ cmd ]

        -- Now this is where we handle the value that the debouncer give us.
        -- In this example, we just add it to the report.
        Saved str ->
            { model | report = str :: model.report } ! []

        -- This is where commands are actually processed by Debounce.update.
        -- The second argument of Debounce.update `(Debounce.takeLast save)`
        -- defines the method to retrieve the debounced values later.
        -- Here it means that we want to keep only the last value (Debounce.takeLast)
        -- and that we will make a `Saved String` Cmd Msg of it thanks to `save`.
        -- You could choose to use all the values since last time with Debounce.takeAll,
        -- providing that the `save` function and `Saved` msg have compatible types.
        DebounceMsg debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        debounceMsg
                        model.debounce
            in
                { model | debounce = debounce } ! [ cmd ]


{-| Make a `Saved String` Cmd Msg to handle the debounced value.
-}
save : String -> Cmd Msg
save str =
    Task.perform Saved (Task.succeed str)


view : Model -> Html Msg
view model =
    H.div []
        [ H.input [ HE.onInput Input ] []
        , H.ul [] (List.map report (List.reverse model.report))
        ]


report : String -> Html msg
report str =
    H.li [] [ H.text (toString str) ]
