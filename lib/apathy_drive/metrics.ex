defmodule ApathyDrive.Metrics do

  def start_link do
    Task.start_link fn ->
      record_metrics()
    end
  end

  def record_metrics do
    "mobiles"
    |> ApathyDrive.PubSub.subscribers
    |> length()
    |> ExStatsD.gauge("mobiles")

    "rooms"
    |> ApathyDrive.PubSub.subscribers
    |> length()
    |> ExStatsD.gauge("rooms")

    ApathyDrive.RoomUnity.controlled_by_counts
    |> Enum.each(fn({controlled_by, count}) ->
         unless controlled_by == nil do
           ApathyDrive.PubSub.broadcast("spirits:online", {:territory, controlled_by, count})
           ExStatsD.gauge(count, "rooms.#{controlled_by}")
         end
       end)

    Task.start fn ->
      Enum.each(:erlang.processes, &:erlang.garbage_collect/1)
    end

    :timer.sleep 10000

    record_metrics()
  end
end