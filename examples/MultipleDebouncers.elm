module MultipleDebouncers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debounce exposing (Debounce)
import Time exposing (..)
import Task exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { name : String
    , nickname : String
    , nameDebouncer : Debounce String
    , nicknameDebouncer : Debounce String
    , report : List String
    }


init : ( Model, Cmd Msg )
init =
    { name = ""
    , nickname = ""
    , nameDebouncer = Debounce.init
    , nicknameDebouncer = Debounce.init
    , report = []
    }
        ! []


type Msg
    = NoOp
    | InputName String
    | InputNickname String
    | Saved String
    | DebounceName Debounce.Msg
    | DebounceNickname Debounce.Msg


debounceConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
debounceConfig debounceMsg =
    { strategy = Debounce.later (1 * second)
    , transform = debounceMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        InputName name ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceName)
                        name
                        model.nameDebouncer
            in
                { model
                    | name = name
                    , nameDebouncer = newDebouncer
                }
                    ! [ cmd ]

        InputNickname nickname ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceNickname)
                        nickname
                        model.nicknameDebouncer
            in
                { model
                    | nickname = nickname
                    , nicknameDebouncer = newDebouncer
                }
                    ! [ cmd ]

        DebounceName msg ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceName)
                        (Debounce.takeLast save)
                        msg
                        model.nameDebouncer
            in
                { model | nameDebouncer = newDebouncer } ! [ cmd ]

        DebounceNickname msg ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceNickname)
                        (Debounce.takeLast save)
                        msg
                        model.nicknameDebouncer
            in
                { model | nicknameDebouncer = newDebouncer } ! [ cmd ]

        Saved s ->
            { model | report = s :: model.report } ! []


save : String -> Cmd Msg
save s =
    Task.perform Saved (Task.succeed s)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Enter your name: " ]
        , input [ value model.name, onInput InputName ] []
        , p [] [ text "Enter your nickname: " ]
        , input [ value model.nickname, onInput InputNickname ] []
        , ul [] (List.map report (List.reverse model.report))
        ]


report : String -> Html msg
report s =
    li [] [ text (toString s) ]
