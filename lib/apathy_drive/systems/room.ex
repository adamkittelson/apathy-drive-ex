defmodule Systems.Room do
  use Systems.Reload
  import Utility

  def display_room_in_scroll(character, room_pid) do
    send_message(character, "scroll", long_room_html(character, room_pid))
  end

  def long_room_html(character, room) do
    directions = room |> exit_directions |> exit_directions_html
    "<div class='room'>#{name_html(room)}#{description_html(room)}#{shop(room)}#{items_html(room)}#{entities_html(character, room)}#{directions}</div>"
  end

  def short_room_html(room) do
    directions = room |> exit_directions |> exit_directions_html
    "<div class='room'>#{name_html(room)}#{directions}</div>"
  end

  def shop(room) do
    case Entity.has_component?(room, Components.Shop) || Entity.has_component?(room, Components.Trainer) do
      true  -> "<p><br><em>Type 'list' to see a list of goods and services sold here.</em><br><br></p>"
      false -> ""
    end
  end

  def room_data(room, character) do
    [
      name: name(room),
      description: description(room),
      exits: exit_directions(room),
      entities: entities(character, room) |> Enum.map(&(&1 |> Components.Name.get_name))
    ]
  end

  def get_current_room(entity) do
    Components.CurrentRoom.get_current_room(entity)
  end

  def exit_directions(room) do
    exits(room) |> Map.keys
  end

  def exit_directions_html([]) do
    "<div class='exits'>Obvious exits: NONE</div>"
  end

  def exit_directions_html(directions) do
    "<div class='exits'>Obvious exits: #{Enum.join(directions, ", ")}</div>"
  end

  def exits(room) do
    Components.Exits.value(room)
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

  def items(_room) do
    []
  end

  def items_html(room) do
    "<div class='items'>#{Enum.join(items(room), ", ")}</div>"
  end

  def entities(character, room) do
    characters = characters_in_room(room, character) |> Enum.reject(&(Components.Spirit.value(&1)))
    monsters   = monsters_in_room(room)
    Enum.concat(characters, monsters)
  end

  def entities_html(character, room) do
    entities = entities(character, room) |> Enum.map(&(&1 |> Components.Name.get_name))
    case Enum.count(entities) do
      0 ->
        ""
      _ ->
        "<div class='entities'><span class='dark-magenta'>Also here:</span> <span class='magenta'>#{Enum.join(entities, ", ")}</span><span class='dark-magenta'>.</span></div>"
    end
  end

  def move(character, direction) do
    current_room = Components.CurrentRoom.get_current_room(character)
    room_exit = current_room |> get_exit_by_direction(direction)
    move(character, current_room, room_exit)
  end

  def move(character, _current_room, nil) do
    send_message(character, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def move(character, current_room, room_exit) do
    destination = Rooms.find_by_id(room_exit["destination"])

    Components.CurrentRoom.set_current_room(character, destination)
    if Components.Spirit.value(character) == false do
      notify_character_left(character, current_room, destination)
      notify_character_entered(character, current_room, destination)
    end
    Components.Hints.deactivate(character, "movement")
    display_room_in_scroll(character, destination)
  end

  def notify_character_entered(character, entered_from, room) do
    direction = get_direction_by_destination(room, entered_from)
    name = Components.Name.get_name(character)
    characters_in_room(room, character) |> Enum.each(fn(character_in_room) ->
      if direction do
        send_message(character_in_room, "scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks into the room from the #{direction}.</span></p>")
      else
        send_message(character_in_room, "scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks into the room.</span></p>")
      end
    end)
  end

  def notify_character_left(character, room, left_to) do
    direction = get_direction_by_destination(room, left_to)
    name = Components.Name.get_name(character)
    characters_in_room(room, character) |> Enum.each(fn(character_in_room) ->
      if direction do
        send_message(character_in_room, "scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks out of the room to the #{direction}.</span></p>")
      else
        send_message(character_in_room, "scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks out of the room.</span></p>")
      end
    end)
  end

  def get_direction_by_destination(room, destination) do
    exits = exits(room)
    exits
    |> Map.keys
    |> Enum.find fn(direction) ->
      Rooms.find_by_id(exits["destination"]) == destination
    end
  end

  def get_exit_by_direction(room, direction) do
    exits(room)[direction]
  end

  def living_in_room(room) do
    Enum.concat(monsters_in_room(room), characters_in_room(room))
  end

  def living_in_room(entities, room) do
    Enum.filter(entities, &(room == Components.CurrentRoom.get_current_room(&1)))
  end

  def characters_in_room(room) do
    Characters.online
    |> living_in_room(room)
  end

  def monsters_in_room(room) do
    Components.Monsters.value(room)
  end

  def characters_in_room(room, character_to_exclude) do
    characters_in_room(room) |> Enum.reject(&(&1 == character_to_exclude))
  end

end
