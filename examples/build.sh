#!/bin/bash

elm make Main.elm --output=../docs/index.html --optimize
elm make ManualUnlock.elm --output=../docs/manual.html --optimize
elm make MultipleDebouncers.elm --output=../docs/multiple.html --optimize
