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

    :timer.sleep 5000

    record_metrics()
  end
end