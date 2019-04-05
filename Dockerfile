FROM quay.io/brightcove/ubuntu12-elixir:elixir-1.8.1-erlang-21.2.6-ubuntu-18.04

ENV REFRESHED_AT 2018-07-01

ADD . /usr/src/app
WORKDIR /usr/src/app

ARG MIX_ENV
ENV MIX_ENV ${MIX_ENV}

RUN mix deps.get

RUN mix compile && mix phx.digest && mix release.clean && mix release --verbose --env=${MIX_ENV}
