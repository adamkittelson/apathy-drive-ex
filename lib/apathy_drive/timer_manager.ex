defmodule ApathyDrive.TimerManager do

  def seconds(seconds), do: seconds |> :timer.seconds |> trunc

  def call_after(%{timers: timers} = entity, {name, time, [module, function, args]}) do
    ref = :erlang.start_timer(time, self, {name, [module, function, args]})

    timers = Map.put(timers, name, ref)

    Map.put(entity, :timers, timers)
  end

  def send_after(%{timers: timers} = entity, {name, time, term}) do
    ref = :erlang.start_timer(time, self, {name, [Kernel, :send, [self, term]]})

    timers = Map.put(timers, name, ref)

    Map.put(entity, :timers, timers)
  end

  def call_every(%{timers: timers} = entity, {name, time, [module, function, args]}) do
    ref = :erlang.start_timer(time, self, {name, time, [module, function, args]})

    timers = Map.put(timers, name, ref)

    Map.put(entity, :timers, timers)
  end

  def send_every(%{timers: timers} = entity, {name, time, term}) do
    ref = :erlang.start_timer(time, self, {name, time, [Kernel, :send, [self, term]]})

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

end
