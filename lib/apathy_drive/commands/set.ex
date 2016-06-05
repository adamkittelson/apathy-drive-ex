defmodule ApathyDrive.Commands.Set do
  use ApathyDrive.Command
  alias ApathyDrive.{Area, Mobile, Repo}

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

  def execute(%Room{area: %Area{name: old_area}} = room, mobile, ["area" | area]) do
    area
    |> Enum.join(" ")
    |> Area.find_by_name
    |> Repo.one
    |> case do
         %Area{} = area ->
           room =
             room
             |> Map.put(:area, area)
             |> Map.put(:area_id, area.id)
             |> Repo.save!

           Mobile.send_scroll(mobile, "<p>Area changed from \"#{old_area}\" to \"#{room.area.name}\".</p>")
           room
         res ->
           IO.inspect(res)
           Mobile.send_scroll(mobile, "<p>Could not find an area named \"#{area}\".")
           room
       end
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
