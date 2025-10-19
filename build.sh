#!/bin/sh

echo "\033[1mBuilding Haskell...\033[0m"
cd "haskell"
if [[ $(command -v cabal) ]]; then 
  eval "cabal build"
else
  echo "Haskell build aborted: cabal not installed"
fi

echo "\033[1mBuilding Rust...\033[0m"
cd "../rust"
if [[ $(command -v cargo) ]]; then 
  eval "cargo build --release"
else
  echo "Rust build aborted: cargo not installed"
fi
