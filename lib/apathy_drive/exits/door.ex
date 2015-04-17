defmodule ApathyDrive.Exits.Door do
  use ApathyDrive.Exits.Doors

  def look(%Room{} = current_room, %Spirit{} = spirit, room_exit) do
    if open?(current_room, room_exit) do
      super(current_room, spirit, room_exit)
    else
      Phoenix.Channel.push spirit.socket, "scroll", %{:html => "<p>The #{name} is closed in that direction!</p>"}
    end
  end

  def look(%Room{} = current_room, %Monster{} = monster, room_exit) do
    if open?(current_room, room_exit) do
      super(current_room, monster, room_exit)
    else
      ApathyDrive.Endpoint.broadcast! "monsters:#{monster.id}", "scroll", %{:html => "<p>The #{name} is closed in that direction!</p>"}
    end
  end

end
