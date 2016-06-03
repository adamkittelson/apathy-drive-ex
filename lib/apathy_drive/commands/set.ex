defmodule ApathyDrive.Commands.Set do
  use ApathyDrive.Command
  alias ApathyDrive.{Mobile, Repo}

  def keywords, do: ["set"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Set what?</p>")
  end

  def execute(mobile, args) when is_pid(mobile) do
    Mobile.set(mobile, args)
  end

  def execute(%Mobile{spirit: %Spirit{admin: true}, room_id: room_id}, args) do
    room_id
    |> RoomServer.find
    |> RoomServer.set(self, args)
  end

  def execute(%Mobile{} = mobile, _args) do
    Mobile.send_scroll(mobile, "<p>You do not have permission to do that.</p>")
  end

  def execute(%Room{area: old_area} = room, mobile, ["area" | area]) do
    room =
      room
      |> Map.put(:area, Enum.join(area, " "))
      |> Repo.save!

    Mobile.send_scroll(mobile, "<p>Area changed from \"#{old_area}\" to \"#{room.area}\".</p>")

    room
  end

  def execute(%Room{name: old_name} = room, mobile, ["name" | room_name]) do
    room =
      room
      |> Map.put(:name, Enum.join(room_name, " "))
      |> Repo.save!

    Mobile.send_scroll(mobile, "<p>Room name changed from \"#{old_name}\" to \"#{room.name}\".</p>")

    room
  end

  def execute(%Room{} = room, mobile, _args) do
    Mobile.send_scroll(mobile, "<p>Set what?</p>")

    room
  end

end
