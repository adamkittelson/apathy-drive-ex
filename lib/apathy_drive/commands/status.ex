defmodule ApathyDrive.Commands.Status do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Level, Mobile}

  def keywords, do: ["st", "stat", "status"]

  def execute(%Room{} = room, %Character{} = character, []) do
    status(character)

    room
  end

  def status(character) do
    hp = Character.hp_at_level(character, character.level)
    max_hp = Mobile.max_hp_at_level(character, character.level)

    mp = Character.mana_at_level(character, character.level)
    max_mp = Mobile.max_mana_at_level(character, character.level)

    modifier = (100 + character.race.race.exp_modifier) / 100
    level = character.level
    exp = trunc(character.experience)
    tolevel = Level.exp_at_level(level, modifier)
    exp = max(0, tolevel - exp)

    Mobile.send_scroll(
      character,
      "<p><span class='cyan'>hp:</span> <span class='white'>#{hp}/#{max_hp}</span> " <>
        "<span class='cyan'>mana:</span> <span class='white'>#{mp}/#{max_mp}</span> " <>
        "<span class='cyan'>experience:</span> <span class='white'>#{exp}</span> " <>
        "<span class='cyan'>mind:</span> #{mind(character)}"
    )
  end

  def mind(character) do
    max_buffer = Character.max_exp_buffer(character)
    buffer = character.exp_buffer

    percent = buffer / max_buffer

    cond do
      percent < 0.05 ->
        "<span class='white'>clear</span>"

      percent < 0.25 ->
        "<span class='white'>almost clear</span>"

      percent < 0.5 ->
        "<span class='white'>slightly fuzzy</span>"

      percent < 0.75 ->
        "<span class='white'>clouded</span>"

      percent < 0.90 ->
        "<span class='white'>very fuzzy</span>"

      :else ->
        "<span class='magenta'>full of facts</span>"
    end
  end
end
