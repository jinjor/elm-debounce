# elm-debounce

Yet another debouncer for Elm.

## How to use

This library follows the Elm Architecture. See the full example [here](https://github.com/jinjor/elm-debounce/blob/master/examples/Main.elm).

```elm
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
  ...


-- This defines how the debouncer should work.
-- Choose the strategy for your use case.
debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.later (1 * second)
  , transform = DebounceMsg
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input s ->
      let
        -- Push your values here.
        (debounce, cmd) =
          Debounce.push debounceConfig s model.debounce
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
            debounceConfig
            (Debounce.takeLast save)
            msg
            model.debounce
      in
        { model | debounce = debounce } ! [ cmd ]

    ...
```

### Multiple Debouncers

If you need to debounce multiple event sources, one approach is to repeat the pattern demonstrated above for each event source. For example, you could define a debouncer and debounce msg for each:

```elm
init : ( Model, Cmd Msg )
init =
  { value = ""
  -- Initialize *multiple* debouncers.
  , fooDebouncer = Debounce.init
  , barDeboucner = Debounce.init
  , report = []
  } ! []

type Msg
  = InputFoo String
  | InputBar String
  | DebounceFoo Debounce.Msg
  | DebounceBar Debounce.Msg
```

You can choose to either have different configs for each event source:

```elm
fooDebounceConfig : Debounce.Config Msg
fooDebounceConfig =
  { strategy = Debounce.later (1 * second)
  , transform = DebounceFoo
  }

barDebounceConfig : Debounce.Config Msg
barDebounceConfig =
  { strategy = Debounce.manual
  , transform = DebounceBar
  }

```

Or to use the same config for both:

```elm
debounceConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
debounceConfig debounceMsg =
    { strategy = Debounce.later (1 * second)
    , transform = debounceMsg
    }
```

Note that the above config function takes your specific debounce msg as it's argument, so for example you might do the following:

```elm
Debounce.push (debounceConfig DebounceFoo) fooValue model.fooDebouncer
```

A full example of this approach can be seen [here](https://github.com/jinjor/elm-debounce/blob/master/examples/MultipleDebouncers.elm).


## LICENSE

BSD3
