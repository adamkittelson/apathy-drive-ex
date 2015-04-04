defmodule TimerManager do

  def call_after(%{timers: timers} = entity, {name, time, function}) do
    ref = :erlang.start_timer(time, self, {name, function})

    timers = Map.put(timers, name, ref)

    Map.put(entity, :timers, timers)
  end

  def call_every(%{timers: timers} = entity, {name, time, function}) do
    ref = :erlang.start_timer(time, self, {name, time, function})

    timers = Map.put(timers, name, ref)

    Map.put(entity, :timers, timers)
  end

  def timers(%{timers: timers}) do
    Map.keys(timers)
  end

  def time_remaining(%{timers: timers}, name) do
    if ref = Map.get(timers, name) do
      :erlang.read_timer(ref)
    end
  end

  def cancel(%{timers: timers}, name) do
    if ref = Map.get(timers, name) do
      :erlang.cancel_timer(ref)
    end
  end

  def execute_function(function) do
    try do
      function.()
    catch
      kind, error ->
        IO.puts Exception.format(kind, error)
        # {fun, arity} = env.function
        # IO.puts """
        # ** BlockTimer apply error, originating from:
        #      #{env.file}:#{env.line} in #{fun}/#{arity}
        #    error:
        #      #{Exception.format(kind, error)}
        # """
    end
  end
end
