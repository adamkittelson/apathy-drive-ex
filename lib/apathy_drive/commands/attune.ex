defmodule ApathyDrive.Commands.Attune do
  use ApathyDrive.Command
  alias ApathyDrive.{Attunement, Character, Mobile, Repo, Room}

  def keywords, do: ["attune"]

  def execute(%Room{items: items} = room, %Character{} = character, _) do
    if obelisk = Enum.find(items, &(&1.id == 802)) do
      attune(character, obelisk, room)
    else
      Mobile.send_scroll(character, "<p>There is no obsidian obelisk here to attune to!</p>")
      room
    end
  end

  defp attune(character, obelisk, room) do
    if Repo.get_by(Attunement, character_id: character.id, obelisk_id: obelisk.instance_id) do
      Mobile.send_scroll(character, "<p>You are already attuned to this obelisk.</p>")

      room
    else
      %Attunement{character_id: character.id, obelisk_id: obelisk.instance_id}
      |> Repo.insert!()

      Mobile.send_scroll(
        character,
        "<p>You are now attuned to this obelisk and may use it as a 'teleport' destination.</p>"
      )

      Room.update_mobile(room, character.ref, fn _room, character ->
        Character.load_attunements(character)
      end)
    end
  end
end
