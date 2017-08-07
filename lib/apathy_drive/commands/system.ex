defmodule ApathyDrive.Commands.System do
  use ApathyDrive.Command
  require Ecto.Query
  alias ApathyDrive.{Area, Room}
  alias ApathyDrive.Commands.System

  def keywords, do: ["system", "sys"]

  def execute(%Room{} = room, %Character{admin: true} = character, args) do
    system(room, character, args)
  end

  def execute(%Room{} = room, %Character{} = character, _args) do
    Mobile.send_scroll(character, "<p>You do not have permission to do that.</p>")
    room
  end

  def system(%Room{} = room, character, ["area", "add", "ally" | ally_name]) do
    System.Area.add_ally(room, character, ally_name)
  end

  def system(%Room{} = room, character, ["area", "add", "enemy" | enemy_name]) do
    System.Area.add_enemy(room, character, enemy_name)
  end

  def system(%Room{} = room, character, ["area", "create" | area]) do
    System.Area.create(room, character, area)
  end

  def system(%Room{} = room, character, ["area", "list"]) do
    System.Area.list(room, character)
  end

  def system(%Room{} = room, character, ["area", "merge" | area]) do
    System.Area.merge(room, character, area)
  end

  def system(%Room{} = room, character, ["area", "set", "level", level]) do
    System.Area.set_level(room, character, level)
  end

  def system(%Room{} = room, character, ["area", "set" | area]) do
    System.Area.set(room, character, area)
  end

  def system(%Room{} = room, character, ["area", "remove", "ally" | ally_name]) do
    System.Area.remove_ally(room, character, ally_name)
  end

  def system(%Room{} = room, character, ["area", "remove", "enemy" | enemy_name]) do
    System.Area.remove_enemy(room, character, enemy_name)
  end

  def system(%Room{} = room, character, ["room", "set", "coords", x, y, z]) do
    System.Room.set_coords(room, character, x, y, z)
  end

  def system(%Room{} = room, character, ["room", "set", "coords" | []]) do
    System.Room.set_coords(room, character)
  end

  def system(%Room{} = room, character, ["room", "set", "name" | room_name]) do
    System.Room.set_name(room, character, room_name)
  end

  def system(%Room{} = room, character, ["goto" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.match_by_name
    |> case do
         %Area{} = area ->
           Ecto.assoc(area, :rooms)
           |> Ecto.Query.where([r], not is_nil(r.coordinates))
           |> Ecto.Query.limit(1)
           |> Ecto.Query.select([:id])
           |> ApathyDrive.Repo.one
           |> case do
                %Room{id: room_id} ->
                  room_exit =
                    %{
                      "kind" => "Action",
                      "destination" => room_id,
                      "mover_message" => "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
                      "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
                      "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
                    }

                  ApathyDrive.Commands.Move.execute(room, character, room_exit)
                _ ->
                  Mobile.send_scroll(character, "<p>#{area.name} has no rooms!</p>")
                  room
              end
         nil ->
           Mobile.send_scroll(character, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def system(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

end
