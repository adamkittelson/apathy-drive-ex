defmodule Commands.Score do
  use ApathyDrive.Command

  def keywords, do: ["score", "stats", "status", "st"]

  def execute(%Spirit{name: nil} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{"Anonymous" |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{spirit.experience}</span></p>")
    |> Spirit.send_scroll("<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{spirit.level}</span></p>")
  end

  def execute(%Spirit{name: name} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{name |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{spirit.experience}</span></p>")
    |> Spirit.send_scroll("<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{spirit.level}</span></p>")
  end

  def execute(%Monster{} = monster, _arguments) do

    Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{monster.name |> String.ljust(18)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{monster.experience}</span></p>")

    monster_level = monster.level |> to_string |> String.ljust(12)
    monster_power = Systems.Trainer.monster_power(monster)

    Monster.send_scroll(monster, "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{monster_level}</span><span class='dark-green'>Devs:</span> <span class='dark-cyan'>#{monster_power}</span></p>")

    hp = String.ljust("#{monster.hp}/#{Monster.max_hp(monster)}", 15)
    mana = "#{monster.mana}/#{Monster.max_mana(monster)}"
    Monster.send_scroll(monster, "<p><span class='dark-green'>HP:</span> <span class='dark-cyan'>#{hp}</span><span class='dark-green'>Mana:</span>  <span class='dark-cyan'>#{mana}</span></p>\n")

    stat_names = ["strength", "agility", "intelligence", "health"]
    chunks = get_chunks(stat_names)
    Enum.each chunks, &display_stats(monster, &1)
    display_effects(monster)
    monster
  end

  def display_effects(%Monster{} = monster) do
    monster.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "effect_message")))
    |> Enum.map(&(&1["effect_message"]))
    |> Enum.each(fn(message) ->
         Monster.send_scroll(monster, "<p>#{message}</p>")
       end)
  end

  defp display_stats(%Monster{} = monster, [stat1, stat2]) do
    Monster.send_scroll(monster, "<p>#{stattext(monster, stat1)} #{stattext(monster, stat2)}</p>")
  end

  defp display_stats(%Monster{} = monster, [stat]) do
    Monster.send_scroll(monster, "<p>#{stattext(monster, stat)}</p>")
  end

  defp stattext(%Monster{} = monster, stat) do
    stat_rating = Monster.modified_stat(monster, stat)
    String.ljust("<span class='dark-green'>#{String.ljust("#{String.capitalize(stat)}:", 15)}</span> <span class='dark-cyan'>#{String.ljust("#{stat_rating}", 7)}</span>", 18)
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
