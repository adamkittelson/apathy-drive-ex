defmodule ApathyDrive.TimerManager do

  def seconds(seconds), do: seconds |> :timer.seconds |> trunc

  def send_after(%{timers: timers} = entity, {name, time, term}) do
    send_at = time + (Timex.Time.now |> Timex.Time.to_milliseconds |> trunc)

    timers = Map.put(timers, name, %{send_at: send_at, message: term})

    send(self, :start_timer)

    Map.put(entity, :timers, timers)
  end

  def apply_timers(%{timers: timers} = entity) do
    now = Timex.Time.now |> Timex.Time.to_milliseconds

    timers
    |> Enum.reduce(entity, fn {name, %{send_at: send_at, message: message}}, updated_entity ->
         if send_at < now do
           send(self(), message)
           update_in(entity.timers, &Map.delete(&1, name))
         else
           entity
         end
       end)
  end

  def timers(%{timers: timers}) do
    Map.keys(timers)
  end

  def time_remaining(%{timers: timers}, name) do
    if timer = Map.get(timers, name) do
      timer.send_at - Timex.Time.to_milliseconds(Timex.Time.now)
    end
  end

  def cancel(%{timers: timers} = entity, name) do
    put_in(entity.timers, Map.delete(timers, name))
  end

  def next_timer(%{timers: timers}) do
    timers
    |> Map.values
    |> Enum.map(&(&1.send_at))
    |> Enum.sort
    |> List.first
  end

end
