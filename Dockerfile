FROM quay.io/adamkittelson/elixir-dockerfile:erlang-21.1-elixir-1.6.5

ENV REFRESHED_AT 2018-07-01

ADD . /usr/src/app
WORKDIR /usr/src/app

ARG MIX_ENV
ENV MIX_ENV ${MIX_ENV}

RUN mix deps.get

RUN mix compile && mix phx.digest && mix release.clean && mix release --verbose --env=${MIX_ENV}
