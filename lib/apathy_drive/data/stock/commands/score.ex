defmodule Commands.Score do
  use Systems.Command

  def keywords, do: ["score", "stats", "status"]

  def execute(spirit, nil, _arguments) do
    if Entity.has_component?(spirit, Components.Name) do
      send_message(spirit, "scroll", "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{Components.Name.value(spirit)}</span> <span class='dark-green'>Possessing:</span> <span class='dark-cyan'>None</span></p>")
    end

    exp = Components.Experience.value(spirit)
    send_message(spirit, "scroll", "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span></p>")

    level = Components.Level.value(spirit)
            |> Integer.to_string
            |> String.ljust(11)

    power  = Systems.Trainer.power(spirit)
    send_message(spirit, "scroll", "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Power:</span> <span class='dark-cyan'>#{power}</span></p>")
  end

  def execute(spirit, monster, _arguments) do
    if Entity.has_component?(spirit, Components.Name) do
      send_message(spirit, "scroll", "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{Components.Name.value(spirit)}</span> <span class='dark-green'>Possessing:</span> <span class='dark-cyan'>#{Components.Name.value(monster)}</span></p>")
    end

    exp = Components.Experience.value(monster)
    send_message(spirit, "scroll", "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span></p>")

    level = Components.Level.value(monster)
            |> Integer.to_string
            |> String.ljust(11)

    power  = Systems.Trainer.power(monster)
    send_message(spirit, "scroll", "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Power:</span> <span class='dark-cyan'>#{power}</span></p>")

    hp = String.ljust("#{Components.HP.value(monster)}/#{Systems.HP.max(monster)}", 15)
    mana = "#{Components.Mana.value(monster)}/#{Systems.Mana.max(monster)}"
    send_message(spirit, "scroll", "<p><span class='dark-green'>HP:</span> <span class='dark-cyan'>#{hp}</span><span class='dark-green'>Mana:</span>  <span class='dark-cyan'>#{mana}</span></p>")

    send_message(spirit, "scroll", "\n")

    stat_names = Components.Stats.value(monster) |> Map.keys
    chunks = get_chunks(stat_names)
    Enum.each chunks, &display_stats(monster, &1)
  end

  defp display_stats(entity, [stat1, stat2]) do
    send_message(entity, "scroll", "<p>#{stattext(entity, stat1)} #{stattext(entity, stat2)}</p>")
  end

  defp display_stats(entity, [stat]) do
    send_message(entity, "scroll", "<p>#{stattext(entity, stat)}</p>")
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
