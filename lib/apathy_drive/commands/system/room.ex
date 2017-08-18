defmodule ApathyDrive.Commands.System.Room do
  alias ApathyDrive.{Mobile, Repo, Room}

  def execute(%Room{} = room, character, ["room", "set", "coords", x, y, z]) do
    set_coords(room, character, x, y, z)
  end

  def execute(%Room{} = room, character, ["room", "set", "coords" | []]) do
    set_coords(room, character)
  end

  def execute(%Room{} = room, character, ["room", "set", "name" | room_name]) do
    set_name(room, character, room_name)
  end

  def set_coords(%Room{coordinates: old_coords} = room, character, x, y, z) do
    x = String.to_integer(x)
    y = String.to_integer(y)
    z = String.to_integer(z)

    room =
      room
      |> Map.put(:coordinates, %{"x" => x, "y" => y, "z" => z})
      |> Repo.save!

    ApathyDriveWeb.Endpoint.broadcast!("map", "room coords change", %{room_id: room.id, x: x, y: y, z: z})

    Mobile.send_scroll(character, "<p>Room coordinates changed from \"#{inspect(old_coords)}\" to \"#{inspect(room.coordinates)}\".</p>")

    room
  end

  def set_coords(%Room{coordinates: old_coords} = room, character) do
    room =
      room
      |> Map.put(:coordinates, nil)
      |> Repo.save!

    ApathyDriveWeb.Endpoint.broadcast!("map", "room coords change", %{room_id: room.id, x: 0, y: 0, z: 0})

    Mobile.send_scroll(character, "<p>Room coordinates unset from \"#{inspect(old_coords)}\".</p>")

    room
  end

  def set_name(%Room{name: old_name} = room, character, room_name) do
    room =
      room
      |> Map.put(:name, Enum.join(room_name, " "))
      |> Repo.save!

    ApathyDriveWeb.Endpoint.broadcast!("map", "room name change", %{room_id: room.id, name: room.name})

    Mobile.send_scroll(character, "<p>Room name changed from \"#{old_name}\" to \"#{room.name}\".</p>")

    room
  end

end
