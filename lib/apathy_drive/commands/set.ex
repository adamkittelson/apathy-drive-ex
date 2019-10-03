defmodule ApathyDrive.Commands.Set do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Race, Room}
  require Ecto.Query

  def keywords, do: ["set"]

  def execute(%Room{} = room, %Character{} = character, []) do
    message = "<p>Valid SET options: RACE</p>"

    Mobile.send_scroll(character, message)

    room
  end

  def execute(%Room{} = room, %Character{} = character, ["race" | args]) do
    query = Enum.join(args, " ")

    case Race.match_by_name(query) do
      nil ->
        Mobile.send_scroll(character, "<p>Sorry! No such race is available.</p>")
        room

      %Race{} = race ->
        set_race(room, character, race)

      list ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(list, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  defp set_race(%Room{} = room, %Character{} = character, %Race{} = race) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      message = "<p>You will be resurrected as a #{race.name} next time you die.</p>"

      Mobile.send_scroll(character, message)

      Map.put(character, :death_race_id, race.id)
    end)
  end
end
