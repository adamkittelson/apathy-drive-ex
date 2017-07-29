defmodule ApathyDrive.Commands.Hire do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Companion, Monster, Party, Repo}

  def keywords, do: ["hire"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    query = Enum.join(arguments)

    case Room.find_mobile_in_room(room, character, query) do
      %Monster{} = monster ->
        if Monster.hireable?(monster, character, room) do
          price = Companion.hire_price(character)

          cond do
            price > character.gold ->
              Mobile.send_scroll(character, "<p>You cannot afford to hire #{Mobile.colored_name(monster, character)}.</p>")
              room
            companion = Character.companion(character, room) ->
              Mobile.send_scroll(character, "<p>You must <span class='green'>dismiss</span> #{Mobile.colored_name(companion, character)} before you can hire #{Mobile.colored_name(monster, character)}.</p>")
              room
            Party.size(room, character) > 5 ->
              Mobile.send_scroll(character, "<p>Your party is already full.</p>")
              room
            true ->
              %Character{id: character.id}
              |> Ecto.Changeset.change(%{gold: character.gold - price})
              |> Repo.update!

              room
              |> Room.update_mobile(character.ref, fn(char) ->
                   update_in(char.gold, &(&1 - price))
                 end)
              |> Companion.convert_for_character(monster, character)
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
