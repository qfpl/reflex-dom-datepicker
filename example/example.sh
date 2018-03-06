#!/usr/bin/env bash
ghcid --command="ghci -ghci-script=example.ghci" -W --test="main" --reload=../css
