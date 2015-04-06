defmodule Commands.Score do
  use ApathyDrive.Command

  def keywords, do: ["score", "stats", "status", "st"]

  def execute(%Spirit{name: nil} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{"Anonymous" |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{spirit.experience}</span></p>")
    |> Spirit.send_scroll("<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{spirit.level}</span></p>")
  end

  def execute(%Spirit{name: name} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{name |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{spirit.experience}</span></p>")

    spirit_level = spirit.level |> to_string |> String.ljust(12)
    spirit_power = Systems.Trainer.spirit_power(spirit)

    Spirit.send_scroll(spirit, "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{spirit_level}</span><span class='dark-green'>Devs:</span> <span class='dark-cyan'>#{spirit_power}</span></p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{monster.name}</span></p>")

    hp = String.ljust("#{monster.hp}/#{monster.max_hp}", 15)
    mana = "#{monster.mana}/#{monster.max_mana}"
    Monster.send_scroll(monster, "<p><span class='dark-green'>HP:</span> <span class='dark-cyan'>#{hp}</span><span class='dark-green'>Mana:</span>  <span class='dark-cyan'>#{mana}</span></p>\n")

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

end
