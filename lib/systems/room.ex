defmodule Systems.Room do
  def display_current_room(player) do
    current_room = get_current_room(player)
    Players.send_message player, ["room", room_data(current_room)]
  end

  def room_data(room) do
    [
      name: Components.Name.get_name(room),
      description: Components.Description.get_description(room),
      exits: Enum.map(Components.Exits.get_exits(room), fn (exit_pid) ->
        Components.Direction.get_direction(exit_pid)
      end)
    ]
  end

  def get_current_room(player) do
    Components.CurrentRoom.get_current_room(player)
  end
end