defmodule ApathyDrive.Commands.System do
  use ApathyDrive.Command
  require Ecto.Query
  alias ApathyDrive.{Area, AreaAlly, AreaEnemy, Character, Mobile, PubSub, Repo, Room}

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

  def system(%Room{area: %Area{level: old_level} = area} = room, character, ["area", "set", "level", level]) do
    area = Area.update_level(area, level)
    PubSub.broadcast!("areas:#{area.id}", {:update_area, area})
    Mobile.send_scroll(character, "<p>#{area.name} updated from level #{old_level} to #{level}.</p>")
    room
  end

  def system(%Room{area: %Area{} = area} = room, character, ["area", "add", "ally" | ally_name]) do
    ally =
      ally_name
      |> Enum.join(" ")
      |> Area.match_by_name

    cond do
      is_nil(ally) ->
        Mobile.send_scroll(character, "<p>Could not find an area called #{Enum.join(ally_name, " ")}</p>")
      ally.id == area.id ->
        Mobile.send_scroll(character, "<p>An area cannot be its own ally.</p>")
      :else ->
        remove_enemy(area, ally)

        Repo.insert_all(AreaAlly, [[area_id: area.id, ally_id: ally.id], [area_id: ally.id, ally_id: area.id]], on_conflict: :nothing)
        |> case do
          {2, nil} ->
            Mobile.send_scroll(character, "<p>#{area.name} is now an ally of #{ally.name}.</p>")
          {0, nil} ->
            Mobile.send_scroll(character, "<p>#{area.name} is already an ally of #{ally.name}.</p>")
          error ->
            Mobile.send_scroll(character, "<p>#{inspect(error)}</p>")
        end
    end
    room
  end

  def system(%Room{area: %Area{} = area} = room, character, ["area", "add", "enemy" | enemy_name]) do
    enemy =
      enemy_name
      |> Enum.join(" ")
      |> Area.match_by_name

    cond do
      is_nil(enemy) ->
        Mobile.send_scroll(character, "<p>Could not find an area called #{Enum.join(enemy_name, " ")}</p>")
      enemy.id == area.id ->
        Mobile.send_scroll(character, "<p>An area cannot be its own enemy.</p>")
      :else ->
        remove_ally(area, enemy)

        Repo.insert_all(AreaEnemy, [[area_id: area.id, enemy_id: enemy.id], [area_id: enemy.id, enemy_id: area.id]], on_conflict: :nothing)
        |> case do
          {2, nil} ->
            Mobile.send_scroll(character, "<p>#{area.name} is now an enemy of #{enemy.name}.</p>")
          {0, nil} ->
            Mobile.send_scroll(character, "<p>#{area.name} is already an enemy of #{enemy.name}.</p>")
          error ->
            Mobile.send_scroll(character, "<p>#{inspect(error)}</p>")
        end
    end
    room
  end

  def system(%Room{area: %Area{} = area} = room, character, ["area", "remove", "enemy" | enemy_name]) do
    enemy =
      enemy_name
      |> Enum.join(" ")
      |> Area.match_by_name

    cond do
      is_nil(enemy) ->
        Mobile.send_scroll(character, "<p>Could not find an area called #{Enum.join(enemy_name, " ")}</p>")
      enemy.id == area.id ->
        Mobile.send_scroll(character, "<p>An area cannot be its own enemy.</p>")
      :else ->
        remove_enemy(area, enemy)

        Mobile.send_scroll(character, "<p>#{area.name} is no longer an enemy of #{enemy.name}.</p>")
    end
    room
  end

  def system(%Room{area: %Area{} = area} = room, character, ["area", "remove", "ally" | ally_name]) do
    ally =
      ally_name
      |> Enum.join(" ")
      |> Area.match_by_name

    cond do
      is_nil(ally) ->
        Mobile.send_scroll(character, "<p>Could not find an area called #{Enum.join(ally_name, " ")}</p>")
      ally.id == area.id ->
        Mobile.send_scroll(character, "<p>An area cannot be its own ally.</p>")
      :else ->
        remove_ally(area, ally)

        Mobile.send_scroll(character, "<p>#{area.name} is no longer an ally of #{ally.name}.</p>")
    end
    room
  end

  def system(%Room{area: %Area{} = old_area} = room, character, ["area", "merge" | area]) do
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
    |> Area.new_area_changeset
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
           area =
             area
             |> Repo.preload(:allies)
             |> Repo.preload(:enemies)

           allies =
             area.allies
             |> Enum.map(&(&1.name))

           enemies =
             area.enemies
             |> Enum.map(&(&1.name))

           Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{to_string(area.level) |> String.rjust(5)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{to_string(room_count) |> String.rjust(5)}</span> <span class='dark-green'>|</span> <span class='black'>#{area.name}</span></p>")
           if Enum.any?(allies) do
             Mobile.send_scroll(character, "<p>              <span class='dark-green'>|</span>   <span class='dark-cyan'>Allies:</span> #{Enum.join(allies, ", ")}</p>")
           end
           if Enum.any?(enemies) do
             Mobile.send_scroll(character, "<p>              <span class='dark-green'>|</span>   <span class='dark-cyan'>Enemies:</span> #{Enum.join(enemies, ", ")}</p>")
           end
         end)

       end)
    room
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

  defp remove_ally(%Area{} = area, %Area{} = ally) do
    Ecto.Query.from(area_ally in AreaAlly, where: area_ally.area_id == ^area.id and area_ally.ally_id == ^ally.id)
    |> Repo.delete_all

    Ecto.Query.from(area_ally in AreaAlly, where: area_ally.area_id == ^ally.id and area_ally.ally_id == ^area.id)
    |> Repo.delete_all
  end

  defp remove_enemy(%Area{} = area, %Area{} = enemy) do
    Ecto.Query.from(area_enemy in AreaEnemy, where: area_enemy.area_id == ^area.id and area_enemy.enemy_id == ^enemy.id)
    |> Repo.delete_all

    Ecto.Query.from(area_enemy in AreaEnemy, where: area_enemy.area_id == ^enemy.id and area_enemy.enemy_id == ^area.id)
    |> Repo.delete_all
  end

end
