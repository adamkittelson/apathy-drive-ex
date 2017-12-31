defmodule ApathyDrive.Commands.Score do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["score", "stats", "status", "st"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    show_score(character, room)
    room
  end

  defp show_score(%Character{} = character, room) do
    score_data = Character.score_data(character, room)
    hits = Enum.join([score_data.hp, trunc(score_data.max_hp)], "/") |> String.pad_trailing(13)
    mana = Enum.join([score_data.mana, trunc(score_data.max_mana)], "/") |> String.pad_trailing(13)

    Mobile.send_scroll(character, "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{String.pad_trailing(score_data.name, 13)}</span><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(score_data.level), 13)}</span><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(score_data.experience), 13)}</span> #{resistance(Enum.at(score_data.resistances, 0))} #{resistance(Enum.at(score_data.resistances, 1))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>Race:</span> <span class='dark-cyan'>#{String.pad_trailing(score_data.race, 13)}</span><span class='dark-green'>Melee DPS:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(Float.round(score_data.melee_dps, 2)), 9)}</span><span class='dark-green'>Perception:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.perception)), 6)}</span> #{resistance(Enum.at(score_data.resistances, 2))} #{resistance(Enum.at(score_data.resistances, 3))}</p>")
    Mobile.send_scroll(character, "<p>                   <span class='dark-green'>Physical Res:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.physical_resistance)), 6)}</span><span class='dark-green'>Stealth:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.stealth)), 9)}</span> #{resistance(Enum.at(score_data.resistances, 4))} #{resistance(Enum.at(score_data.resistances, 5))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>Hits:</span> <span class='dark-cyan'>#{hits}</span><span class='dark-green'>Magical Dmg:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.magical_damage)), 7)}</span><span class='dark-green'>Block:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.block)), 11)}</span> #{resistance(Enum.at(score_data.resistances, 6))} #{resistance(Enum.at(score_data.resistances, 7))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>Mana:</span> <span class='dark-cyan'>#{mana}</span><span class='dark-green'>Magical Res:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.magical_resistance)), 7)}</span><span class='dark-green'>Accuracy:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.accuracy)), 8)}</span> #{resistance(Enum.at(score_data.resistances, 8))} #{resistance(Enum.at(score_data.resistances, 9))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>#{String.pad_leading("Dodge:", 45)}</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.dodge)), 11)}</span> #{resistance(Enum.at(score_data.resistances, 10))} #{resistance(Enum.at(score_data.resistances, 11))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>Strength:</span>  <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.strength)), 7)}</span> <span class='dark-green'>Agility:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.agility)), 11)}</span><span class='dark-green'>Spellcasting:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.spellcasting)), 4)}</span> #{resistance(Enum.at(score_data.resistances, 12))} #{resistance(Enum.at(score_data.resistances, 13))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>Intellect:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.intellect)), 7)}</span> <span class='dark-green'>Health:</span>  <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.health)), 11)}</span><span class='dark-green'>Crits:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.crits)), 11)}</span> #{resistance(Enum.at(score_data.resistances, 14))} #{resistance(Enum.at(score_data.resistances, 15))}</p>")
    Mobile.send_scroll(character, "<p><span class='dark-green'>Willpower:</span> <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.willpower)), 7)}</span> <span class='dark-green'>Charm:</span>   <span class='dark-cyan'>#{String.pad_trailing(to_string(trunc(score_data.charm)), 11)}</span><span class='dark-green'>Item Level:</span> <span class='dark-cyan'>#{String.pad_leading(to_string(trunc(score_data.item_level)), 6)}</span>#{resistance(Enum.at(score_data.resistances, 16))} #{resistance(Enum.at(score_data.resistances, 17))}</p>")

    Enum.each(score_data.effects, fn(effect_message) ->
      Mobile.send_scroll(character, "<p>#{effect_message}</p>")
    end)
  end

  defp resistance(nil), do: ""
  defp resistance({name, value}) do
    "   <span class='dark-green'>#{String.pad_trailing("Resist" <> name <> ":", 16)}</span> <span class='dark-cyan'>#{String.pad_leading(to_string(value), 3)}</span>"
  end

end
