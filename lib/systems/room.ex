defmodule Systems.Room do
  def display_current_room(player) do
    Players.send_message player, ["room", get_current_room(player) |> room_data]
  end

  def room_data(room) do
    [
      name: name(room),
      description: description(room),
      exits: exit_directions(room)
    ]
  end

  def get_current_room(player) do
    character = Components.Login.get_character(player)
    Components.CurrentRoom.get_current_room(character)
  end

  def exit_directions(room) do
    exits(room) |> Enum.map fn (exit_pid) ->
      Components.Direction.get_direction(exit_pid)
    end
  end

  def exits(room) do
    Components.Exits.get_exits(room)
  end

  def description(room) do
    Components.Description.get_description(room)
  end

  def name(room) do
    Components.Name.get_name(room)
  end

  def move(player, direction) do
    destination = player |> get_current_room
                         |> get_exit_by_direction(direction)
                         |> Components.Destination.get_destination

    ApathyDrive.Entity.notify(player, {:set_current_room, destination})
    display_current_room(player)
  end

  def get_exit_by_direction(room, direction) do
    exits(room) |> Enum.find fn (room_exit) ->
      Components.Direction.get_direction(room_exit) == direction
    end
  end

end
