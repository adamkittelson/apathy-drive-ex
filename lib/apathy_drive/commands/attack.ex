defmodule ApathyDrive.Commands.Attack do
  use ApathyDrive.Command
  alias ApathyDrive.{Match, Mobile, Party}

  def keywords, do: ["a", "attack", "k", "kill"]

  def execute(%Room{} = room, %{} = character, []) do
    Mobile.send_scroll(character, "<p>Attack whom?</p>")
    room
  end

  def execute(%Room{} = room, %{} = character, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase()

    target =
      room.mobiles
      |> Map.values()
      |> Enum.reject(&(&1.ref in Party.refs(room, character)))
      |> Enum.reject(&(&1.sneaking && !(&1.ref in character.detected_characters)))
      |> Match.one(:keyword_starts_with, query)

    room =
      Room.update_mobile(room, character.ref, fn _room, %{} = attacker ->
        attack(attacker, target)
      end)

    Room.update_hp_bar(room, character.ref)
    Room.update_mana_bar(room, character.ref)

    room
  end

  def attack(%{} = character, nil) do
    Mobile.send_scroll(character, "<p>Attack whom?</p>")
  end

  def attack(%{casting: nil} = character, %{ref: target_ref}) do
    character
    |> Map.put(:attack_target, target_ref)
    |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
  end

  def attack(%{} = character, %{ref: target_ref}) do
    character
    |> Map.put(:attack_target, target_ref)
    |> Mobile.send_scroll(
      "<p><span class='dark-red'>You interrupt your other ability.</span></p>"
    )
    |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
  end
end
