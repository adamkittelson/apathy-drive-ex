defmodule ApathyDrive.Commands.System do
  use ApathyDrive.Command
  require Ecto.Query
  alias ApathyDrive.{Area, Character, Mobile, PubSub, Repo, Room}

  def keywords, do: ["system", "sys"]

  def execute(%Room{} = room, %Character{admin: true} = character, args) do
    system(room, character, args)
  end

  def execute(%Room{} = room, %Character{} = character, _args) do
    Mobile.send_scroll(character, "<p>You do not have permission to do that.</p>")
    room
  end

  def system(%Room{id: id} = room, %Character{admin: true} = character, ["edit", "room"]) do
    send(character.socket, {:open_tab, "/admin/rooms/#{id}"})
    room
  end

  def system(%Room{area: %Area{level: old_level} = area} = room, character, ["set", "area", "level", level]) do
    area = Area.update_level(area, level)
    PubSub.broadcast!("areas:#{area.id}", {:update_area, area})
    Mobile.send_scroll(character, "<p>#{area.name} updated from level #{old_level} to #{level}.</p>")
    room
  end

  def system(%Room{area: %Area{} = old_area} = room, character, ["merge", "area" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.find_by_name
    |> Repo.one
    |> case do
         %Area{} = area ->
           PubSub.broadcast!("areas:#{old_area.id}", {:update_area, area})
           Mobile.send_scroll(character, "<p>#{old_area.name} merged into #{area.name}.</p>")
           room
         nil ->
           Mobile.send_scroll(character, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def system(%Room{area: %Area{} = old_area} = room, character, ["set", "area" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.find_by_name
    |> Repo.one
    |> case do
         %Area{} = area ->
           room = Room.update_area(room, area)

           Mobile.send_scroll(character, "<p>Area changed from \"#{old_area.name}\"(#{old_area.level}) to \"#{room.area.name}\"(#{room.area.level}).</p>")
           room
         nil ->
           Mobile.send_scroll(character, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def system(%Room{} = room, character, ["create", "area" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.changeset
    |> Repo.insert
    |> case do
         {:ok, %Area{name: name} = area} ->
           room = Room.update_area(room, area)
           Mobile.send_scroll(character, "<p>\"#{name}\" created!</p>")
           room
         {:error, %Ecto.Changeset{errors: errors}} ->
           Enum.each(errors, fn {field, error} ->
             message = ApathyDrive.ErrorHelpers.translate_error(error)
             Mobile.send_scroll(character, "<p>Error: #{field} #{message}</p>")
           end)
           room
       end
  end

  def system(%Room{name: old_name} = room, character, ["set", "room", "name" | room_name]) do
    room =
      room
      |> Map.put(:name, Enum.join(room_name, " "))
      |> Repo.save!

    ApathyDrive.Endpoint.broadcast!("map", "room name change", %{room_id: room.id, name: room.name})

    Mobile.send_scroll(character, "<p>Room name changed from \"#{old_name}\" to \"#{room.name}\".</p>")

    room
  end

  def system(%Room{coordinates: old_coords} = room, character, ["set", "room", "coords", x, y, z]) do
    x = String.to_integer(x)
    y = String.to_integer(y)
    z = String.to_integer(z)

    room =
      room
      |> Map.put(:coordinates, %{"x" => x, "y" => y, "z" => z})
      |> Repo.save!

    ApathyDrive.Endpoint.broadcast!("map", "room coords change", %{room_id: room.id, x: x, y: y, z: z})

    Mobile.send_scroll(character, "<p>Room coordinates changed from \"#{inspect(old_coords)}\" to \"#{inspect(room.coordinates)}\".</p>")

    room
  end

  def system(%Room{coordinates: old_coords} = room, character, ["set", "room", "coords" | []]) do
    room =
      room
      |> Map.put(:coordinates, nil)
      |> Repo.save!

    ApathyDrive.Endpoint.broadcast!("map", "room coords change", %{room_id: room.id, x: 0, y: 0, z: 0})

    Mobile.send_scroll(character, "<p>Room coordinates unset from \"#{inspect(old_coords)}\".</p>")

    room
  end

  def system(%Room{} = room, character, ["list", "areas"]) do
    Area.list_with_room_counts
    |> Repo.all
    |> Enum.chunk(10)
    |> Enum.each(fn chunk ->
         Mobile.send_scroll(character, "<p><span class='dark-magenta'>Level</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Rooms</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Area</span></p>")
         Enum.each(chunk, fn [area, room_count] ->
           Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{to_string(area.level) |> String.rjust(5)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{to_string(room_count) |> String.rjust(5)}</span> <span class='dark-green'>|</span> <span class='black'>#{area.name}</span></p>")
         end)

       end)
    room
  end

  def system(%Room{} = room, character, ["goto" | area]) do
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
