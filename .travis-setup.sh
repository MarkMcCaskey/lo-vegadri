#!/bin/sh

set -eux

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_stack_linux() {
 # curl -SL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
	curl -sSL https://get.haskellstack.org/ | sh	
}

# We need stack to generate cabal files with precise bounds, even for cabal
# builds.
mkdir -p ~/.local/bin;
travis_retry fetch_stack_linux

travis_retry stack --no-terminal setup;
