defmodule ApathyDrive.Commands.System do
  use ApathyDrive.Command
  require Ecto.Query
  alias ApathyDrive.{Area, Monster, PubSub, Repo, Room}

  def keywords, do: ["system", "sys"]

  def execute(%Room{} = room, %Monster{spirit: %Spirit{admin: true}} = monster, args) do
    system(room, monster, args)
  end

  def execute(%Room{} = room, %Monster{} = monster, _args) do
    Monster.send_scroll(monster, "<p>You do not have permission to do that.</p>")
    room
  end

  def system(%Room{id: id} = room, %Monster{spirit: %Spirit{admin: true}} = monster, ["edit", "room"]) do
    send(monster.socket, {:open_tab, "/admin/rooms/#{id}"})
    room
  end

  def system(%Room{area: %Area{level: old_level} = area} = room, monster, ["set", "area", "level", level]) do
    area = Area.update_level(area, level)
    PubSub.broadcast!("areas:#{area.id}", {:update_area, area})
    Monster.send_scroll(monster, "<p>#{area.name} updated from level #{old_level} to #{level}.</p>")
    room
  end

  def system(%Room{area: %Area{} = old_area} = room, monster, ["merge", "area" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.find_by_name
    |> Repo.one
    |> case do
         %Area{} = area ->
           PubSub.broadcast!("areas:#{old_area.id}", {:update_area, area})
           Monster.send_scroll(monster, "<p>#{old_area.name} merged into #{area.name}.</p>")
           room
         nil ->
           Monster.send_scroll(monster, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def system(%Room{area: %Area{} = old_area} = room, monster, ["set", "area" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.find_by_name
    |> Repo.one
    |> case do
         %Area{} = area ->
           room = Room.update_area(room, area)

           Monster.send_scroll(monster, "<p>Area changed from \"#{old_area.name}\"(#{old_area.level}) to \"#{room.area.name}\"(#{room.area.level}).</p>")
           room
         nil ->
           Monster.send_scroll(monster, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def system(%Room{} = room, monster, ["create", "area" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.changeset
    |> Repo.insert
    |> case do
         {:ok, %Area{name: name} = area} ->
           room = Room.update_area(room, area)
           Monster.send_scroll(monster, "<p>\"#{name}\" created!</p>")
           room
         {:error, %Ecto.Changeset{errors: errors}} ->
           Enum.each(errors, fn {field, error} ->
             message = ApathyDrive.ErrorHelpers.translate_error(error)
             Monster.send_scroll(monster, "<p>Error: #{field} #{message}</p>")
           end)
           room
       end
  end

  def system(%Room{name: old_name} = room, monster, ["set", "room", "name" | room_name]) do
    room =
      room
      |> Map.put(:name, Enum.join(room_name, " "))
      |> Repo.save!

    ApathyDrive.Endpoint.broadcast!("map", "room name change", %{room_id: room.id, name: room.name})

    Monster.send_scroll(monster, "<p>Room name changed from \"#{old_name}\" to \"#{room.name}\".</p>")

    room
  end

  def system(%Room{coordinates: old_coords} = room, monster, ["set", "room", "coords", x, y, z]) do
    x = String.to_integer(x)
    y = String.to_integer(y)
    z = String.to_integer(z)

    room =
      room
      |> Map.put(:coordinates, %{"x" => x, "y" => y, "z" => z})
      |> Repo.save!

    ApathyDrive.Endpoint.broadcast!("map", "room coords change", %{room_id: room.id, x: x, y: y, z: z})

    Monster.send_scroll(monster, "<p>Room coordinates changed from \"#{inspect(old_coords)}\" to \"#{inspect(room.coordinates)}\".</p>")

    room
  end

  def system(%Room{coordinates: old_coords} = room, monster, ["set", "room", "coords" | []]) do
    room =
      room
      |> Map.put(:coordinates, nil)
      |> Repo.save!

    ApathyDrive.Endpoint.broadcast!("map", "room coords change", %{room_id: room.id, x: 0, y: 0, z: 0})

    Monster.send_scroll(monster, "<p>Room coordinates unset from \"#{inspect(old_coords)}\".</p>")

    room
  end

  def system(%Room{} = room, monster, ["list", "areas"]) do
    Area.list_with_room_counts
    |> Repo.all
    |> Enum.chunk(10)
    |> Enum.each(fn chunk ->
         Monster.send_scroll(monster, "<p><span class='dark-magenta'>Level</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Rooms</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Area</span></p>")
         Enum.each(chunk, fn [area, room_count] ->
           Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{to_string(area.level) |> String.rjust(5)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{to_string(room_count) |> String.rjust(5)}</span> <span class='dark-green'>|</span> <span class='black'>#{area.name}</span></p>")
         end)

       end)
    room
  end

  def system(%Room{} = room, monster, ["goto" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.find_by_name
    |> Repo.one
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

                  ApathyDrive.Commands.Move.execute(room, monster, room_exit)
                _ ->
                  Monster.send_scroll(monster, "<p>#{area.name} has no rooms!</p>")
                  room
              end
         nil ->
           Monster.send_scroll(monster, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def system(%Room{} = room, monster, _args) do
    Monster.send_scroll(monster, "<p>Invalid system command.</p>")

    room
  end

end
