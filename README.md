# elm-debounce

Yet another debouncer for Elm.

## How to use

This library follows the Elm Architecture.
See the [full example](https://github.com/jinjor/elm-debounce/blob/master/examples/Main.elm) and [how it looks like (demo)](https://jinjor.github.io/elm-debounce/).


```elm
init : ( Model, Cmd Msg )
init =
  { value = ""
  , debounce = Debounce.init -- Initialize the debouncer.
  , report = []
  } ! []


type Msg
  = Input String
  | DebounceMsg Debounce.Msg
  ...


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
    Input str ->
      let
        -- Push your values. They are not immediately processed.
        (debounce, cmd) =
          Debounce.push debounceConfig str model.debounce
      in
        { model
          | value = str
          , debounce = debounce
        } ! [ cmd ]

    -- This is where debounced values are actually processed and sent as Cmd.
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

    ...
```

## LICENSE

BSD3
