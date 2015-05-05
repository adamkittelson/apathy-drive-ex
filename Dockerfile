FROM trenpixster/elixir:1.0.4

RUN echo "deb http://packages.erlang-solutions.com/ubuntu trusty contrib" >> /etc/apt/sources.list && \
    apt-get -qq update && apt-get install -y \
    postgresql-client && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mix local.rebar --force && mix local.hex --force

ADD . /usr/src/app
WORKDIR /usr/src/app

ENV MIX_ENV prod
RUN mix deps.get && mix compile && mix phoenix.digest

ENV ELIXIR_ERL_OPTIONS -kernel inet_dist_listen_min 49000 -kernel inet_dist_listen_max 49004 +K true +A 64

CMD elixir -S mix phoenix.server
