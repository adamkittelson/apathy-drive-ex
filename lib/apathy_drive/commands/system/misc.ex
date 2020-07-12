defmodule ApathyDrive.Commands.System.Misc do
  alias ApathyDrive.{Area, Character, Mobile, PubSub, Race, Repo, Room}
  require Ecto.Query

  def execute(%Room{} = room, character, ["goto" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.match_by_name()
    |> case do
      %Area{} = area ->
        Ecto.assoc(area, :rooms)
        |> Ecto.Query.where([r], not is_nil(r.coordinates))
        |> Ecto.Query.limit(1)
        |> Ecto.Query.select([:id])
        |> ApathyDrive.Repo.one()
        |> case do
          %Room{id: room_id} ->
            room_exit = %{
              "kind" => "Action",
              "destination" => room_id,
              "mover_message" =>
                "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
              "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
              "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
            }

            ApathyDrive.Commands.Move.execute(room, character, room_exit, 0)

          _ ->
            Mobile.send_scroll(character, "<p>#{area.name} has no rooms!</p>")
            room
        end

      nil ->
        Mobile.send_scroll(character, "<p>Could not find an area named \"#{area}\".")
        room
    end
  end

  def execute(%Room{} = room, character, ["racechange", character_name | race_name]) do
    race_name = Enum.join(race_name, " ")

    char =
      Character
      |> Repo.get_by(name: character_name)

    race =
      Race
      |> Repo.get_by(name: race_name)

    cond do
      is_nil(char) ->
        Mobile.send_scroll(character, "<p>There is no character called #{character_name}.</p>")

      is_nil(race) ->
        Mobile.send_scroll(character, "<p>There is no race called #{race_name}.</p>")

      :else ->
        char
        |> Ecto.Changeset.change(%{race_id: race.id})
        |> Repo.update!()

        PubSub.broadcast("rooms", {:change_race, %{character: char.id, race: race.id}})

        Mobile.send_scroll(character, "<p>#{char.name}'s race has been changed to #{race.name}.")
    end

    room
  end

  def execute(%Room{} = room, character, args) do
    Mobile.send_scroll(character, "<p>Invalid system command: #{inspect(args)}</p>")

    room
  end
end
