#!/bin/bash
set -eo pipefail

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./*.stack-work/*" \
    -not -path "./sketches/*" \
    -not -path "./.git/*")
}

function build_and_test {
  stack build \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --test \
  --fast
}

function build {
  stack build \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --fast
}

function haddock {
  stack haddock \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --fast
}

function fork_haddock {
  mkdir -p ".haddock.stack-work"

  stack haddock \
  --work-dir ".haddock.stack-work" \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --fast \
  &> .haddock.stack-work/log &
}

function demo {
  stack exec demo
}

format
fork_haddock
build_and_test
