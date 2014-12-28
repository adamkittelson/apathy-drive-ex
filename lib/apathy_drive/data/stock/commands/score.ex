defmodule Commands.Score do
  use Systems.Command

  def keywords, do: ["score", "stats", "status", "st"]

  def execute(spirit, nil, _arguments) do
    if Entity.has_component?(spirit, Components.Name) do
      send_message(spirit, "scroll", "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{Components.Name.value(spirit) |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{Components.Experience.value(spirit)}</span></p>")
    else
      send_message(spirit, "scroll", "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{"Anonymous" |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{Components.Experience.value(spirit)}</span></p>")
    end

    send_message(spirit, "scroll", "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{Components.Level.value(spirit)}</span></p>")
  end

  def execute(spirit, monster, _arguments) do
    if Entity.has_component?(spirit, Components.Name) do
      send_message(spirit, "scroll", "<p><span class='dark-cyan'>#{Components.Name.value(spirit)}</span></p>")
    else
      send_message(spirit, "scroll", "<p><span class='dark-cyan'>Spirit</span></p>")
    end

    send_message(spirit, "scroll", "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{Components.Level.value(spirit) |> to_string |> String.ljust(11)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{Components.Experience.value(spirit)}</span>\n\n</p>")

    send_message(spirit, "scroll", "<p><span class='dark-cyan'>#{Components.Name.value(monster) |> String.ljust(18)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{Components.Experience.value(monster)}</span></p>")

    monster_level = Components.Level.value(monster) |> to_string |> String.ljust(12)
    monster_power = Systems.Trainer.monster_power(monster)

    send_message(spirit, "scroll", "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{monster_level}</span><span class='dark-green'>Devs:</span> <span class='dark-cyan'>#{monster_power}</span></p>")

    hp = String.ljust("#{Components.HP.value(monster)}/#{Systems.HP.max(monster)}", 15)
    mana = "#{Components.Mana.value(monster)}/#{Systems.Mana.max(monster)}"
    send_message(spirit, "scroll", "<p><span class='dark-green'>HP:</span> <span class='dark-cyan'>#{hp}</span><span class='dark-green'>Mana:</span>  <span class='dark-cyan'>#{mana}</span></p>")

    send_message(spirit, "scroll", "\n")

    stat_names = Components.Stats.value(monster) |> Map.keys
    chunks = get_chunks(stat_names)
    Enum.each chunks, &display_stats(monster, &1)
    display_effects(monster)
  end

  def display_effects(monster) do
    monster
    |> Components.Effects.value
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, :effect_message)))
    |> Enum.map(&(&1[:effect_message]))
    |> Enum.each(fn(message) ->
         send_message(monster, "scroll", "<p>#{message}</p>")
       end)
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
