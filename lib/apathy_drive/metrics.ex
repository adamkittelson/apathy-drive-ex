defmodule ApathyDrive.Metrics do

  def start_link do
    Task.start_link fn ->
      record_metrics()
    end
  end

  def record_metrics(gc_ran? \\ false) do
    "monsters"
    |> ApathyDrive.PubSub.subscribers
    |> length()
    |> ExStatsD.gauge("monsters")

    room_count =

      "rooms"
      |> ApathyDrive.PubSub.subscribers
      |> length()

    room_count
    |> ExStatsD.gauge("rooms")

    ApathyDrive.RoomUnity.controlled_by_counts
    |> Enum.each(fn({controlled_by, count}) ->
         unless controlled_by == nil do
           ExStatsD.gauge(count, "rooms.#{controlled_by}")
         end
       end)



    gc_ran? =
      if room_count > 19_700 and !gc_ran? do
        Task.start fn ->
          Enum.each(:erlang.processes, &:erlang.garbage_collect/1)
        end
        true
      else
        gc_ran?
      end

    :timer.sleep 10000

    record_metrics(gc_ran?)
  end
end