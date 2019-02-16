defmodule ApathyDrive.Commands.System.Area do
  alias ApathyDrive.{Area, Mobile, PubSub, Repo, Room}
  require Ecto.Query

  def execute(%Room{} = room, character, ["create" | area]) do
    create(room, character, area)
  end

  def execute(%Room{} = room, character, ["list"]) do
    list(room, character)
  end

  def execute(%Room{} = room, character, ["merge" | area]) do
    merge(room, character, area)
  end

  def execute(%Room{} = room, character, ["set", "level", level]) do
    set_level(room, character, level)
  end

  def execute(%Room{} = room, character, ["set" | area]) do
    set(room, character, area)
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

  def create(%Room{} = room, character, area_name) do
    area = Enum.join(area_name, " ")

    area
    |> Area.new_area_changeset()
    |> Repo.insert()
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
    Area.list_with_room_counts()
    |> Repo.all()
    |> Enum.chunk_every(10)
    |> Enum.each(fn chunk ->
      Mobile.send_scroll(
        character,
        "<p><span class='dark-magenta'>Level</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Rooms</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Area</span></p>"
      )

      Enum.each(chunk, fn [area, room_count] ->
        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'>#{to_string(area.level) |> String.pad_leading(5)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{
            to_string(room_count) |> String.pad_leading(5)
          }</span> <span class='dark-green'>|</span> <span class='black'>#{area.name}</span></p>"
        )
      end)
    end)

    room
  end

  def merge(%Room{area: %Area{} = old_area} = room, character, area_name) do
    area = Enum.join(area_name, " ")

    area
    |> Area.find_by_name()
    |> Repo.one()
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

    Mobile.send_scroll(
      character,
      "<p>#{area.name} updated from level #{old_level} to #{level}.</p>"
    )

    room
  end

  def set(%Room{area: %Area{} = old_area} = room, character, area_name) do
    area = Enum.join(area_name, " ")

    area
    |> Area.find_by_name()
    |> Repo.one()
    |> case do
      %Area{} = area ->
        room = Room.update_area(room, area)

        Mobile.send_scroll(
          character,
          "<p>Area changed from \"#{old_area.name}\"(#{old_area.level}) to \"#{room.area.name}\"(#{
            room.area.level
          }).</p>"
        )

        room

      nil ->
        Mobile.send_scroll(character, "<p>Could not find an area named \"#{area}\".")
        room
    end
  end
end
