defmodule ApathyDrive.Commands.Set do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Race, Repo, Room}
  require Ecto.Query

  def keywords, do: ["set"]

  def execute(%Room{} = room, %Character{} = character, ["combat", type, "color", color])
      when type in ["attack", "target", "spectator"] do
    Mobile.send_scroll(
      character,
      "<p style='color: #{color};'>#{String.capitalize(type)} color set.</p>"
    )

    character =
      case type do
        "attack" ->
          character
          |> Ecto.Changeset.change(%{attack_color: color})
          |> Repo.update!()

        "target" ->
          character
          |> Ecto.Changeset.change(%{target_color: color})
          |> Repo.update!()

        "spectator" ->
          character
          |> Ecto.Changeset.change(%{spectator_color: color})
          |> Repo.update!()
      end

    put_in(room.mobiles[character.ref], character)
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

  def execute(%Room{} = room, %Character{} = character, _args) do
    message = "<p>Valid SET option examples:
  race dwarf
  combat attack color blue
  combat target color red
  combat spectator color #ff00ff</p>"

    Mobile.send_scroll(character, message)

    room
  end

  defp set_race(%Room{} = room, %Character{} = character, %Race{} = race) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      message = "<p>You will be resurrected as a #{race.name} next time you die.</p>"

      Mobile.send_scroll(character, message)

      Map.put(character, :death_race_id, race.id)
    end)
  end
end
