FROM quay.io/brightcove/ubuntu12-elixir:elixir-1.9.0-erlang-22.0.7-ubuntu-18.04

ENV REFRESHED_AT 2018-07-01

ARG MIX_ENV
ENV MIX_ENV ${MIX_ENV}

COPY mix.* VERSION.yml _build /usr/src/tempapp/

WORKDIR /usr/src/tempapp
RUN mix deps.get && mix deps.compile

COPY . /usr/src/app

RUN rm -r /usr/src/app/_build
RUN mv /usr/src/tempapp/_build /usr/src/app/

WORKDIR /usr/src/app

RUN mix deps.get && mix compile && mix phx.digest && mix release apathy_drive
