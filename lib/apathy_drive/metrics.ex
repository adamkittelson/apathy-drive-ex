defmodule ApathyDrive.Metrics do
  alias ApathyDrive.{Directory, Statix}
  
  def child_spec(_arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, []}
    }
  end

  def start_link do
    Task.start_link(fn ->
      record_metrics()
    end)
  end

  def record_metrics() do
    :scheduler.utilization(10)
    |> Enum.each(fn
      {:normal, scheduler, percent, _percent_charlist} ->
        Statix.gauge(
          "scheduler_utilization",
          trunc(percent * 100),
          tags: ["scheduler:#{scheduler}"]
        )

      _ ->
        :noop
    end)

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

    memory_used_in_mb = trunc(:erlang.memory()[:total] / 1024 / 1024)

    Statix.gauge("memory_used", memory_used_in_mb)

    record_metrics()
  end
end
