defmodule ApathyDrive.Exits.Door do
  use ApathyDrive.Exits.Doors

  def look(%Room{} = current_room, %Spirit{} = spirit, room_exit) do
    if open?(current_room, room_exit) do
      super(spirit, current_room, room_exit)
    else
      Phoenix.Channel.reply spirit.socket, "scroll", %{:html => "<p>The #{name} is closed in that direction!</p>"}
    end
  end

  def look(%Room{} = current_room, %Monster{} = monster, room_exit) do
    if open?(current_room, room_exit) do
      super(monster, current_room, room_exit)
    else
      ApathyDrive.Endpoint.broadcast! "monsters:#{monster.id}", "scroll", %{:html => "<p>The #{name} is closed in that direction!</p>"}
    end
  end

end
