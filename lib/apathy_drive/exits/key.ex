defmodule ApathyDrive.Exits.Key do
  use ApathyDrive.Exits.Doors

  def look(%Spirit{} = spirit, %Room{} = current_room, room_exit) do
    if open?(current_room, room_exit) do
      super(spirit, current_room, room_exit)
    else
      Phoenix.Channel.reply spirit.socket, "scroll", %{:html => "<p>The #{name} is closed in that direction!</p>"}
    end
  end

  def look(%Monster{} = monster, %Room{} = current_room, room_exit) do
    if open?(current_room, room_exit) do
      super(monster, current_room, room_exit)
    else
      Phoenix.Channel.broadcast "monsters:#{monster.id}", "scroll", %{:html => "<p>The #{name} is closed in that direction!</p>"}
    end
  end

  def bash?(monster, room_exit) do
    unlocked?(Parent.of(monster), room_exit)
  end

end
