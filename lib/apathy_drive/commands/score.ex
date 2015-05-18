defmodule Commands.Score do
  use ApathyDrive.Command

  def keywords, do: ["score", "stats", "status", "st"]

  def execute(%Spirit{name: name} = spirit, _arguments) do
    color = case spirit.alignment do
      "good" ->
        "white"
      "neutral" ->
        "dark-cyan"
      "evil" ->
        "magenta"
    end
    Spirit.send_scroll(spirit, "<p><span class='dark-green'>Name:</span> <span class='#{color}'>#{name |> String.ljust(12)}</span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>#{spirit.experience}</span></p>")

    spirit_level = spirit.level |> to_string |> String.ljust(12)

    Spirit.send_scroll(spirit, "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{spirit_level}</span></p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    ac = Monster.effect_bonus(monster, "ac")
    name = String.ljust(monster.name, 30)

    Monster.send_scroll(monster, "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{name}</span></p>")

    hp = String.ljust("#{monster.hp}/#{monster.max_hp}", 15)
    mana = String.ljust("#{monster.mana}/#{monster.max_mana}", 10)
    attack =
      monster
      |> Monster.modified_skill("attack")
      |> to_string
      |> String.ljust(11)
    dodge =
      monster
      |> Monster.modified_skill("dodge")
      |> to_string
      |> String.ljust(10)

    pr = Monster.effect_bonus(monster, "physical resistance")
    mr = Monster.effect_bonus(monster, "magical resistance")

    Monster.send_scroll(monster, "<p><span class='dark-green'>HP:</span> <span class='dark-cyan'>#{hp}</span><span class='dark-green'>Mana:</span>  <span class='dark-cyan'>#{mana}</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Attack:</span> <span class='dark-cyan'>#{attack}</span><span class='dark-green'>Dodge:</span> <span class='dark-cyan'>#{dodge}</span></p>\n")

    Monster.send_scroll(monster, "<p><span class='dark-cyan'>Defense:</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Armour:</span> <span class='dark-cyan'>#{ac}</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Physical Resistance:</span>  <span class='dark-cyan'>#{pr}</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Magical Resistance:</span>  <span class='dark-cyan'>#{mr}</span></p>\n")


    ice       = monster |> Monster.effect_bonus("ice resistance")       |> to_string |> String.ljust(4)
    fire      = monster |> Monster.effect_bonus("fire resistance")      |> to_string |> String.ljust(4)
    stone     = monster |> Monster.effect_bonus("stone resistance")     |> to_string |> String.ljust(4)
    lightning = monster |> Monster.effect_bonus("lightning resistance") |> to_string |> String.ljust(4)
    normal    = monster |> Monster.effect_bonus("normal resistance")    |> to_string |> String.ljust(4)
    water     = monster |> Monster.effect_bonus("water resistance")     |> to_string |> String.ljust(4)
    poison    = monster |> Monster.effect_bonus("poison resistance")    |> to_string |> String.ljust(4)

    Monster.send_scroll(monster, "<p><span class='dark-cyan'>Elemental Resistances:</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Normal:</span> <span class='dark-cyan'>#{normal}</span><span class='dark-green'>Stone:</span> <span class='dark-cyan'>#{stone}</span><span class='dark-green'>Fire:</span>  <span class='dark-cyan'>#{fire}</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Poison:</span> <span class='dark-cyan'>#{poison}</span><span class='dark-green'>Water:</span> <span class='dark-cyan'>#{water}</span><span class='dark-green'>Ice:</span>   <span class='dark-cyan'>#{ice}</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>Lightning:</span> <span class='dark-cyan'>#{lightning}</span></p>")

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
