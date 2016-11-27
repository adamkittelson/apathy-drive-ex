defmodule ApathyDrive.Commands.Dismiss do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Companion, Monster, Repo, RoomMonster, TimerManager}

  def keywords, do: ["dimiss", "fire"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    query = Enum.join(arguments)

    case Room.find_mobile_in_room(room, character, query) do
      %Companion{} = companion ->
        %RoomMonster{id: companion.room_monster_id}
        |> Repo.delete!

        Mobile.send_scroll(character, "<p>You release #{Mobile.colored_name(companion, character)} from your service, and they wander off into the sunset.</p>")

        update_in(room.mobiles, &Map.delete(&1, companion.ref))
      _ ->
        Mobile.send_scroll(character, "<p>You don't see #{query} here!</p>")
        room
    end
  end
end
