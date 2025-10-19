#!/bin/sh

LANG=$2
if [[ ! "$LANG" ]]; then
  echo "run.sh: no language specified"
  exit 1
elif [[ "$LANG" != "haskell" && "$LANG" != "rust" ]]; then
  echo "run.sh: invalid language $LANG"
  exit 1
fi

BIN_NAME=$1
BIN_PATH=""
if [[ "$LANG" == "haskell" || "$LANG" == "nil" ]]; then
  cd "haskell"
  BIN_PATH=$(eval "cabal list-bin exe:$BIN_NAME")
  cd ".."
fi
if [[ "$LANG" == "rust" || "$LANG" == "nil" ]]; then
  cd "rust"
  BIN_PATH="$(pwd)/target/release/$BIN_NAME"
  cd ".."
fi

"$BIN_PATH"
