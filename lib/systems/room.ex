defmodule Systems.Room do
  def display_room(player, room_pid) do
    Players.send_message player, ["room", room_pid |> room_data]
  end

  def display_room_in_scroll(player, room_pid) do
    Players.send_message player, ["scroll", long_room_html(room_pid)]
  end

  def display_short_room_in_scroll(player, room_pid) do
    Players.send_message player, ["scroll", short_room_html(room_pid)]
  end

  def long_room_html(room) do
    "<div class='room'>#{name_html(room)}#{description_html(room)}#{items_html(room)}#{entities_html(room)}#{exit_directions_html(room)}</div>"
  end

  def short_room_html(room) do
    "<div class='room'>#{name_html(room)}#{exit_directions_html(room)}</div>"
  end

  def room_data(room) do
    [
      name: name(room),
      description: description(room),
      exits: exit_directions(room)
    ]
  end

  def get_current_room(entity) do
    Components.CurrentRoom.get_current_room(entity)
  end

  def exit_directions(room) do
    exits(room) |> Enum.map fn (exit_pid) ->
      Components.Direction.get_direction(exit_pid)
    end
  end

  def exit_directions_html(room) do
    directions = exit_directions(room)
    if Enum.any? directions do
      "<div class='exits'>Obvious exits: #{Enum.join(directions, ", ")}</div>"
    else
      "<div class='exits'>Obvious exits: NONE</div>"
    end
  end

  def exits(room) do
    Components.Exits.get_exits(room)
  end

  def description(room) do
    Components.Description.get_description(room)
  end

  def description_html(room) do
    "<div class='description'>#{description(room)}</div>"
  end

  def name(room) do
    Components.Name.get_name(room)
  end

  def name_html(room) do
    "<div class='title'>#{name(room)}</div>"
  end

  def items(room) do
    []
  end

  def items_html(room) do
    "<div class='items'>#{Enum.join(items(room), ", ")}</div>"
  end

  def entities(room) do
    []
  end

  def entities_html(room) do
    "<div class='items'>#{Enum.join(entities(room), ", ")}</div>"
  end

  def move(player, character, direction) do
    current_room = get_current_room(character)
    destination = current_room |> get_exit_by_direction(direction)
                               |> Components.Destination.get_destination

    ApathyDrive.Entity.notify(character, {:set_current_room, destination})
    display_room(player, destination)
    display_short_room_in_scroll(player, destination)
  end

  def get_exit_by_direction(room, direction) do
    exits(room) |> Enum.find fn (room_exit) ->
      Components.Direction.get_direction(room_exit) == direction
    end
  end

end
