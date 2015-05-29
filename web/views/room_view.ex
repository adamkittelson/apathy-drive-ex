defmodule ApathyDrive.RoomView do
  use ApathyDrive.Web, :view

  def look_directions(conn, %Room{} = room) do
    case exit_directions(conn, room) do
      [] ->
        "<div class='exits'>Obvious exits: NONE</div>"
      directions ->
        "<div class='exits'>Obvious exits: #{Enum.join(directions, ", ")}</div>"
    end
  end

  def exit_directions(conn, %Room{} = room) do
    room.exits
    |> Enum.map(fn(room_exit) ->
         direction_link(conn, room_exit, :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".display_direction(room, room_exit))
       end)
    |> Enum.reject(&(&1 == nil))
  end

  defp direction_link(conn, room_exit, nil) do
    {:safe, link} = link("#{room_exit["kind"]}-#{room_exit["direction"]}", to: room_path(conn, :show, room_exit["destination"]))
    Enum.join(link)
  end

  defp direction_link(conn, room_exit, direction) do
    {:safe, link} = link(direction, to: room_path(conn, :show, room_exit["destination"]))
    Enum.join(link)
  end
end
