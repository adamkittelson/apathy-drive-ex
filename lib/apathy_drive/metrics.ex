defmodule ApathyDrive.Metrics do
  alias ApathyDrive.{Directory, Statix}

  def start_link do
    Task.start_link(fn ->
      record_metrics()
    end)
  end

  def record_metrics() do
    Directory.list_characters()
    |> Enum.reduce(%{}, fn
      %{game: game}, list ->
        list
        |> Map.put_new(game, 0)
        |> update_in([game], &(&1 + 1))

      %{}, list ->
        list
        |> Map.put_new("Apotheosis", 0)
        |> update_in(["Apotheosis"], &(&1 + 1))
    end)
    |> Enum.each(fn {game, count} ->
      Statix.gauge("players", count, tags: ["game:#{String.downcase(game)}"])
    end)

    room_count =
      "rooms"
      |> ApathyDrive.PubSub.subscribers()
      |> length()

    Statix.gauge("rooms", room_count)

    :timer.sleep(10000)

    record_metrics()
  end
end
