defmodule ApathyDrive.Commands.Possess do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match, Mobile, Monster, Repo, Room, RoomMonster}

  def keywords, do: ["possess"]

  def execute(%Room{} = room, %{} = character, []) do
    Mobile.send_scroll(character, "<p>Possess what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase()

    target =
      room.mobiles
      |> Map.values()
      |> Enum.reject(&(&1.sneaking && !(&1.ref in character.detected_characters)))
      |> Match.one(:keyword_starts_with, query)

    possess(room, character, target)
  end

  def execute(%Room{} = room, %Monster{name: name} = character, _arguments) do
    Mobile.send_scroll(character, "<p>You are already possessing #{name}!</p>")

    room
  end

  def possess(room, character, %Monster{possessing_character: nil} = target) do
    {:ok, room} =
      Repo.transaction(fn ->
        room =
          Room.update_mobile(room, character.ref, fn _room, character ->
            character
            |> Ecto.Changeset.change(%{
              possessed_monster_id: target.room_monster_id
            })
            |> Repo.update!()
          end)

        room =
          Room.update_mobile(room, target.ref, fn _room, monster ->
            %RoomMonster{id: monster.room_monster_id}
            |> Ecto.Changeset.change(%{possessing_character_id: character.id})
            |> Repo.update!()

            Map.put(monster, :possessing_character, room.mobiles[character.ref])
          end)

        room
      end)

    Mobile.send_scroll(character, "<p>You possess #{target.name}.</p>")

    room
  end

  def possess(room, character, monster) do
    Mobile.send_scroll(character, "<p>#{monster.name} is already possessed.</p>")

    room
  end
end
