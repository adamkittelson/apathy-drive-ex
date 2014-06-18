defmodule Commands.Score do
  use Systems.Command

  def keywords, do: ["score", "stats", "status"]

  def execute(entity, _arguments) do
    if Entity.has_component?(entity, Components.Name) do
      Components.Player.send_message(entity, ["scroll", "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{Components.Name.value(entity)}</span></p>"])
    end

    exp = Components.Experience.value(entity)
    cond do
      Entity.has_component?(entity, Components.Race) ->
        race = entity
               |> Components.Race.value
               |> Components.Name.value
               |> String.ljust(11)
        Components.Player.send_message(entity, ["scroll", "<p><span class='dark-green'>Race:</span> <span class='dark-cyan'>#{race}</span> <span class='dark-green'>Exp:</span>  <span class='dark-cyan'>#{exp}</span></p>"])
      Components.Spirit.value(entity) ->
        Components.Player.send_message(entity, ["scroll", "<p><span class='dark-green'>Race:</span> <span class='dark-cyan'>Spirit     </span> <span class='dark-green'>Exp:</span>  <span class='dark-cyan'>#{exp}</span></p>"])
    end

    level = Components.Level.value(entity)
            |> Integer.to_string
            |> String.ljust(10)

    devs  = Systems.Trainer.devs(entity)
    Components.Player.send_message(entity, ["scroll", "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Devs:</span> <span class='dark-cyan'>#{devs}</span></p>"])

    if Entity.has_component?(entity, Components.HP) do
      Components.Player.send_message(entity, ["scroll", "<p><span class='dark-green'>Hits:</span> <span class='dark-cyan'>#{Components.HP.value(entity)}/#{Systems.HP.max_hp(entity)}</span></p>"])
    end

    Components.Player.send_message(entity, ["scroll", "\n"])

    if Entity.has_component?(entity, Components.Stats) do
      stat_names = Components.Stats.value(entity) |> Map.keys
      chunks = get_chunks(stat_names)
      Enum.each chunks, &display_stats(entity, &1)
    end
  end

  defp display_stats(entity, [stat1, stat2]) do
    Components.Player.send_message(entity, ["scroll", "<p>#{stattext(entity, stat1)} #{stattext(entity, stat2)}</p>"])
  end

  defp display_stats(entity, [stat]) do
    Components.Player.send_message(entity, ["scroll", "<p>#{stattext(entity, stat)}</p>"])
  end

  defp stattext(entity, stat) do
    stat_rating = Systems.Stat.modified(entity, stat)
    String.ljust("<span class='dark-green'>#{String.ljust("#{String.capitalize(stat)}:", 10)}</span> <span class='dark-cyan'>#{String.ljust("#{stat_rating}", 7)}</span>", 18)
  end

  defp get_chunks([]), do: []
  defp get_chunks(stats) do
    chunks = Enum.chunk(stats, 2)
    last_stat = stats |> List.last
    if List.flatten(chunks) |> Enum.member?(last_stat) do
      chunks
    else
      [[last_stat] | chunks |> Enum.reverse] |> Enum.reverse
    end
  end

end
