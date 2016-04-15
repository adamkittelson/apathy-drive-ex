FROM trenpixster/elixir:1.2.0

ADD . /usr/src/app
WORKDIR /usr/src/app

ARG MIX_ENV
ENV MIX_ENV ${MIX_ENV}

RUN mix deps.clean --all && mix clean && mix deps.get && mix compile && mix phoenix.digest && mix release.clean && mix release --verbosity=verbose