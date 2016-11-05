defmodule ApathyDrive.Commands.Attack do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["a", "attack", "k", "kill"]

  def execute(%Room{} = room, %{} = character, []) do
    Mobile.send_scroll(character, "<p>Attack whom?</p>")
    room
  end

  def execute(%Room{} = room, %{} = character, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase

    target =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.ref == character.ref)
      |> Match.one(:name_contains, query)

    Room.update_mobile(room, character.ref, fn %{} = attacker ->
      attack(attacker, target)
    end)
  end

  def attack(%{} = character, nil) do
    Mobile.send_scroll(character, "<p>Attack whom?</p>")
  end

  def attack(%{} = character, %{ref: target_ref}) do
    character
    |> Map.put(:attack_target, target_ref)
    |> Mobile.initiate_combat
    |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
  end

end
