defmodule ApathyDrive.Commands.Hire do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Companion, Monster, Repo, RoomMonster, TimerManager}

  def keywords, do: ["hire"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    query = Enum.join(arguments)

    case Room.find_mobile_in_room(room, character, query) do
      %Monster{} = monster ->
        if Monster.hireable?(monster, character, room) do
          price = Companion.hire_price(character)

          if price > character.gold do
            Mobile.send_scroll(character, "<p>You cannot afford to hire #{Mobile.colored_name(monster, character)}.</p>")
            room
          else
            %Character{id: character.id}
            |> Ecto.Changeset.change(%{gold: character.gold - price})
            |> Repo.update!

            %RoomMonster{id: monster.room_monster_id}
            |> Ecto.Changeset.change(%{character_id: character.id})
            |> Repo.update!

            Room.update_mobile(room, character.ref, fn(char) ->
              update_in(char.gold, &(&1 - price))
              |> Mobile.send_scroll("<p>#{monster.name} started to follow you</p>")
            end)
          end
        else
          Mobile.send_scroll(character, "<p>#{Mobile.colored_name(monster, character)} has no interest in joining your party.</p>")
          room
        end
      _ ->
        Mobile.send_scroll(character, "<p>You don't see #{query} here!</p>")
        room
    end
  end
end
