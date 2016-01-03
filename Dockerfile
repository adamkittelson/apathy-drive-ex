FROM phusion/baseimage:0.9.16

ENV REFRESHED_AT 2015-12-29

RUN echo /root > /etc/container_environment/HOME

RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

CMD ["/sbin/my_init"]

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

WORKDIR /tmp

RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN echo "deb http://packages.erlang-solutions.com/ubuntu trusty contrib" >> /etc/apt/sources.list && \
    apt-key adv --fetch-keys http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc && \
    apt-get -qq update && apt-get install -y \
    postgresql-client && \
    esl-erlang=1:18.1 \
    git \
    unzip \
    build-essential \
    wget && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /elixir
RUN wget -q https://github.com/elixir-lang/elixir/releases/download/v1.2.0/Precompiled.zip && \
    unzip Precompiled.zip && \
    rm -f Precompiled.zip && \
    ln -s /elixir/bin/elixirc /usr/local/bin/elixirc && \
    ln -s /elixir/bin/elixir /usr/local/bin/elixir && \
    ln -s /elixir/bin/mix /usr/local/bin/mix && \
    ln -s /elixir/bin/iex /usr/local/bin/iex

# Install local Elixir hex and rebar
RUN /usr/local/bin/mix local.hex --force && \
    /usr/local/bin/mix local.rebar --force

WORKDIR /

RUN mix local.rebar --force && mix local.hex --force

ADD . /usr/src/app
WORKDIR /usr/src/app

ENV MIX_ENV prod
RUN mix deps.get && mix compile && mix phoenix.digest

ENV ELIXIR_ERL_OPTIONS -kernel inet_dist_listen_min 49000 -kernel inet_dist_listen_max 49004 +K true +A 64

CMD elixir -S mix phoenix.server
