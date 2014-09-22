defmodule Systems.Room do
  use Systems.Reload
  alias Systems.Monster
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

  def get_current_room(entity) do
    Parent.of(entity)
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

  def items(room) do
    Components.Items.get_items(room) |> Enum.map(&(Components.Name.value(&1)))
  end

  def items_html(room) do
    items = items(room)

    case Enum.count(items) do
      0 ->
        ""
      _ ->
        "<div class='items'>You notice #{Enum.join(items(room), ", ")} here.</div>"
    end
  end

  def entities(entity, room) do
    monsters_in_room(room) |> Enum.reject(&(entity == &1))
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
    current_room = Parent.of(character)
    room_exit = current_room |> get_exit_by_direction(direction)
    move(character, current_room, room_exit)
  end

  def move(character, _current_room, nil) do
    send_message(character, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def move(entity, current_room, room_exit) do
    if Entity.has_component?(entity, Components.Spirit) do
      move_spirit(entity, current_room, room_exit)
    else
      move_monster(entity, current_room, room_exit)
    end
  end

  def move_spirit(spirit, current_room, room_exit) do
    destination = Rooms.find_by_id(room_exit["destination"])
    Components.Characters.remove_character(current_room, spirit)
    Components.Characters.add_character(destination, spirit)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save!(spirit)
    Components.Hints.deactivate(spirit, "movement")
    display_room_in_scroll(spirit, destination)
  end

  def move_monster(monster, current_room, room_exit) do
    character = Possession.possessor(monster)
    destination = Rooms.find_by_id(room_exit["destination"])
    Components.Monsters.remove_monster(current_room, monster)
    Components.Monsters.add_monster(destination, monster)
    Components.Characters.remove_character(current_room, character)
    Components.Characters.add_character(destination, character)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save!(character)
    notify_monster_left(monster, current_room, destination)
    notify_monster_entered(monster, current_room, destination)
    Components.Hints.deactivate(character, "movement")
    display_room_in_scroll(monster, destination)
  end

  def notify_monster_entered(monster, entered_from, room) do
    direction = get_direction_by_destination(room, entered_from)
    if direction do
      Monster.display_enter_message(room, monster, direction)
    else
      Monster.display_enter_message(room, monster)
    end
  end

  def notify_monster_left(monster, room, left_to) do
    direction = get_direction_by_destination(room, left_to)
    if direction do
      Monster.display_exit_message(room, monster, direction)
    else
      Monster.display_exit_message(room, monster)
    end
  end

  def get_direction_by_destination(room, destination) do
    exits = exits(room)
    exits
    |> Map.keys
    |> Enum.find fn(direction) ->
      Rooms.find_by_id(exits[direction]["destination"]) == destination
    end
  end

  def get_exit_by_direction(room, direction) do
    exits(room)[direction]
  end

  def living_in_room(room) do
    characters = characters_in_room(room) |> Enum.reject(&(Components.Spirit.value(&1)))
    Enum.concat(monsters_in_room(room), characters)
  end

  def living_in_room(entities, room) do
    Enum.filter(entities, &(room == Parent.of(&1)))
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
