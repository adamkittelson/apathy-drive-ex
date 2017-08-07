defmodule ApathyDrive.Commands.System.Area do
  alias ApathyDrive.{Area, AreaAlly, AreaEnemy, Mobile, PubSub, Repo, Room}
  require Ecto.Query

  def add_ally(%Room{area: %Area{} = area} = room, character, ally_name) do
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
            PubSub.broadcast!("areas:#{area.id}", :reload_reputations)
            PubSub.broadcast!("areas:#{ally.id}", :reload_reputations)
          {0, nil} ->
            Mobile.send_scroll(character, "<p>#{area.name} is already an ally of #{ally.name}.</p>")
          error ->
            Mobile.send_scroll(character, "<p>#{inspect(error)}</p>")
        end
    end
    room
  end

  def add_enemy(%Room{area: %Area{} = area} = room, character, enemy_name) do
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
            PubSub.broadcast!("areas:#{area.id}", :reload_reputations)
            PubSub.broadcast!("areas:#{enemy.id}", :reload_reputations)
          {0, nil} ->
            Mobile.send_scroll(character, "<p>#{area.name} is already an enemy of #{enemy.name}.</p>")
          error ->
            Mobile.send_scroll(character, "<p>#{inspect(error)}</p>")
        end
    end
    room
  end

  def create(%Room{} = room, character, area_name) do
    area = Enum.join(area_name, " ")

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
             message = ApathyDriveWeb.ErrorHelpers.translate_error(error)
             Mobile.send_scroll(character, "<p>Error: #{field} #{message}</p>")
           end)
           room
       end
  end

  def list(%Room{} = room, character) do
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

           Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{to_string(area.level) |> String.pad_leading(5)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{to_string(room_count) |> String.pad_leading(5)}</span> <span class='dark-green'>|</span> <span class='black'>#{area.name}</span></p>")
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

  def merge(%Room{area: %Area{} = old_area} = room, character, area_name) do
    area = Enum.join(area_name, " ")

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

  def set_level(%Room{area: %Area{level: old_level} = area} = room, character, level) do
    area = Area.update_level(area, level)
    PubSub.broadcast!("areas:#{area.id}", {:update_area, area})
    Mobile.send_scroll(character, "<p>#{area.name} updated from level #{old_level} to #{level}.</p>")
    room
  end

  def remove_ally(%Room{area: %Area{} = area} = room, character, ally_name) do
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
        PubSub.broadcast!("areas:#{area.id}", :reload_reputations)
        PubSub.broadcast!("areas:#{ally.id}", :reload_reputations)

        Mobile.send_scroll(character, "<p>#{area.name} is no longer an ally of #{ally.name}.</p>")
    end
    room
  end

  def remove_enemy(%Room{area: %Area{} = area} = room, character, enemy_name) do
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
        PubSub.broadcast!("areas:#{area.id}", :reload_reputations)
        PubSub.broadcast!("areas:#{enemy.id}", :reload_reputations)

        Mobile.send_scroll(character, "<p>#{area.name} is no longer an enemy of #{enemy.name}.</p>")
    end
    room
  end

  def set(%Room{area: %Area{} = old_area} = room, character, area_name) do
    area = Enum.join(area_name, " ")

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
