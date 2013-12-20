#!/usr/bin/env sh

if [ ! -f deps ]; then
  grunt sass && grunt coffee && mix deps.get && mix compile --force
fi

case $1 in
    "--no-shell")
        exec elixir --detached -S mix run --no-halt
        ;;
    *)
        exec iex -S mix
        ;;
esac