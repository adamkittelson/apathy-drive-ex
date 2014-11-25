defmodule Systems.Exits.Key do
  use Systems.Exits.Doors

  def look(spirit, monster, current_room, room_exit) do
    if open?(current_room, room_exit) do
      super(spirit, monster, current_room, room_exit)
    else
      send_message(spirit, "scroll", "<p>The #{name} is closed in that direction!</p>")
    end
  end

  def bash?(monster, room_exit) do
    unlocked?(Parent.of(monster), room_exit)
  end

end
