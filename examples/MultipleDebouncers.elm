module MultipleDebouncers exposing (..)

import Browser
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    { name : String
    , nickname : String
    , nameDebouncer : Debounce String
    , nicknameDebouncer : Debounce String
    , report : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , nickname = ""
      , nameDebouncer = Debounce.init
      , nicknameDebouncer = Debounce.init
      , report = []
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | InputName String
    | InputNickname String
    | Saved String
    | DebounceName Debounce.Msg
    | DebounceNickname Debounce.Msg


debounceConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
debounceConfig debounceMsg =
    { strategy = Debounce.later 1000
    , transform = debounceMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        InputName name ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceName)
                        name
                        model.nameDebouncer
            in
            ( { model
                | name = name
                , nameDebouncer = newDebouncer
              }
            , cmd
            )

        InputNickname nickname ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceNickname)
                        nickname
                        model.nicknameDebouncer
            in
            ( { model
                | nickname = nickname
                , nicknameDebouncer = newDebouncer
              }
            , cmd
            )

        DebounceName msg_ ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceName)
                        (Debounce.takeLast save)
                        msg_
                        model.nameDebouncer
            in
            ( { model | nameDebouncer = newDebouncer }
            , cmd
            )

        DebounceNickname msg_ ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceNickname)
                        (Debounce.takeLast save)
                        msg_
                        model.nicknameDebouncer
            in
            ( { model | nicknameDebouncer = newDebouncer }
            , cmd
            )

        Saved s ->
            ( { model | report = s :: model.report }
            , Cmd.none
            )


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
    li [] [ text s ]
