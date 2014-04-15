defmodule Systems.Room do
  def display_room(character, room_pid) do
    Components.Player.send_message(character, ["room", room_pid |> room_data(character)])
  end

  def display_room_in_scroll(character, room_pid) do
    Components.Player.send_message(character, ["scroll", long_room_html(character, room_pid)])
  end

  def long_room_html(character, room) do
    "<div class='room'>#{name_html(room)}#{description_html(room)}#{items_html(room)}#{entities_html(character, room)}#{exit_directions_html(room)}</div>"
  end

  def short_room_html(room) do
    "<div class='room'>#{name_html(room)}#{exit_directions_html(room)}</div>"
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

  def items(_room) do
    []
  end

  def items_html(room) do
    "<div class='items'>#{Enum.join(items(room), ", ")}</div>"
  end

  def entities(character, room) do
    characters = characters_in_room(room, character)
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

  def move(player, character, direction) do
    current_room = get_current_room(character)
    destination = current_room |> get_exit_by_direction(direction)
                               |> Components.Destination.get_destination

    ApathyDrive.Entity.notify(character, {:set_current_room, destination})
    notify_character_left(character, current_room, destination)
    Components.Player.send_message(character, ["scroll", "<p><span class='dark-green'>You move to the #{direction}.</span></p>"])
    display_room(character, destination)
    notify_character_entered(character, current_room, destination)
  end

  def notify_character_entered(character, entered_from, room) do
    direction = get_direction_by_destination(room, entered_from)
    name = Components.Name.get_name(character)
    characters_in_room(room, character) |> Enum.each(fn(character_in_room) ->
      display_room(character_in_room, room)
      if direction do
        Components.Player.send_message(character_in_room, ["scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks into the room from the #{direction}.</span></p>"])
      else
        Components.Player.send_message(character_in_room, ["scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks into the room.</span></p>"])
      end
    end)
  end

  def notify_character_left(character, room, left_to) do
    direction = get_direction_by_destination(room, left_to)
    name = Components.Name.get_name(character)
    characters_in_room(room, character) |> Enum.each(fn(character_in_room) ->
      display_room(character_in_room, room)
      if direction do
        Components.Player.send_message(character_in_room, ["scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks out of the room to the #{direction}.</span></p>"])
      else
        Components.Player.send_message(character_in_room, ["scroll", "<p><span class='red'>#{name}</span> <span class='dark-green'>walks out of the room.</span></p>"])
      end
    end)
  end

  def get_exit_by_destination(room, destination) do
    exits(room) |> Enum.find fn (room_exit) ->
      Components.Destination.get_destination(room_exit) == destination
    end
  end

  def get_direction_by_destination(room, destination) do
    room_exit = get_exit_by_destination(room, destination)
    if room_exit do
      Components.Direction.get_direction(room_exit)
    end
  end

  def get_exit_by_direction(room, direction) do
    exits(room) |> Enum.find fn (room_exit) ->
      Components.Direction.get_direction(room_exit) == direction
    end
  end

  def entities_in_room(entities, room) do
    Enum.filter(entities, fn(entity) ->
      room == Components.CurrentRoom.get_current_room(entity)
    end)
  end

  def characters_in_room(room) do
    Characters.online |> entities_in_room(room)
  end

  def monsters_in_room(room) do
    Components.Monsters.value(room)
  end

  def characters_in_room(room, character_to_exclude) do
    characters_in_room(room) |> Enum.reject(&(&1 == character_to_exclude))
  end

  def find_character_by_name(room, character_name) do
    Enum.find(characters_in_room(room), fn(character) ->
      String.downcase(Components.Name.get_name(character)) == String.downcase(character_name)
    end)
  end

  def initialize_lair_spawning(room) do
    ApathyDrive.Entity.add_component(room, Components.Monsters, [])
    if ApathyDrive.Entity.list_components(room) |> Enum.member?(Components.LairFrequency) do
      :timer.apply_interval(Components.LairFrequency.value(room) * 1000 * 60, Systems.Room, :spawn_lair, [room])
    end
  end

  def spawn_lair(room) do
    if (Systems.Room.monsters_in_room(room) |> Enum.count) < Components.LairSize.value(room) do
      room |> select_lair_monster
           |> Systems.Monster.spawn_monster(room)
      spawn_lair(room)
    end
  end

  def select_lair_monster(room) do
    :random.seed(:os.timestamp)
    monster = room |> Components.LairMonsters.value
                   |> Enum.shuffle
                   |> Enum.first
    IO.puts "Spawning #{Components.Name.get_name(monster)} in #{Components.Name.get_name(room)}"
    monster
  end

end
