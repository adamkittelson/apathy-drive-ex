defmodule Systems.Room do
  use Systems.Reload
  import Utility

  def display_room_in_scroll(spirit, monster, room) do
    send_message(spirit, "scroll", long_room_html(spirit, monster, room))
  end

  def long_room_html(spirit, nil, room) do
    directions = room |> Room.exit_directions |> exit_directions_html
    "<div class='room'>#{name_html(room)}#{description_html(room)}#{shop(room)}#{items_html(room)}#{monsters_html(spirit, room)}#{directions}</div>"
  end

  def long_room_html(spirit, monster, room) do
    light = light_level(monster)
    if light > -200 and light < 200 do
      directions = Room.exit_directions(room) |> exit_directions_html
      "<div class='room'>#{name_html(room)}#{description_html(room)}#{shop(room)}#{items_html(room)}#{monsters_html(monster, room)}#{directions}#{light(monster)}</div>"
    else
      "<div class='room'>#{light(monster)}</div>"
    end
  end

  def short_room_html(character, room) do
    if light_level(character) > -200 do
      directions = Room.exit_directions(room) |> exit_directions_html
      "<div class='room'>#{name_html(room)}#{directions}</div>"
    else
      "<div class='room'>#{light(character)}</div>"
    end
  end

  def shop(room) do
    if Room.shop?(room) or Room.trainer?(room) do
      "<p><br><em>Type 'list' to see a list of goods and services sold here.</em><br><br></p>"
    else
      ""
    end
  end

  def exit_directions_html([]) do
    "<div class='exits'>Obvious exits: NONE</div>"
  end

  def exit_directions_html(directions) do
    "<div class='exits'>Obvious exits: #{Enum.join(directions, ", ")}</div>"
  end

  def description_html(room) do
    "<div class='description'>#{Room.description(room)}</div>"
  end

  def name_html(room) do
    "<div class='title'>#{Room.name(room)}</div>"
  end

  def items(room) do
    Room.items(room) |> Enum.map(&Systems.Item.item_name/1)
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

  def light_desc(light_level)  when light_level < -1000, do: "<p>You are blind.</p>"
  def light_desc(light_level)  when light_level <= -300, do: "<p>The room is pitch black - you can't see anything</p>"
  def light_desc(light_level)  when light_level <= -200, do: "<p>The room is very dark - you can't see anything</p>"
  def light_desc(light_level)  when light_level <= -100, do: "<p>The room is barely visible</p>"
  def light_desc(light_level)  when light_level <=  -25, do: "<p>The room is dimly lit</p>"
  def light_desc(light_level)  when light_level >=  300, do: "<p>The room is blindingly bright - you can't see anything</p>"
  def light_desc(light_level)  when light_level >=  200, do: "<p>The room is painfully bright - you can't see anything</p>"
  def light_desc(light_level)  when light_level >=  100, do: "<p>The room is dazzlingly bright</p>"
  def light_desc(light_level)  when light_level >=   25, do: "<p>The room is brightly lit</p>"
  def light_desc(light_level), do: nil

  def light(monster) do
    light_level(monster)
    |> light_desc
  end

  def light_level(%{room_id: nil}, monster), do: 0
  def light_level(monster) do
    room = Monster.room(monster)
    alignment = Components.Alignment.value(monster)
    light     = room.light + light_in_room(room) + light_on_monsters(room)

    cond do
      alignment > 0 and light < 0 ->
        min(0, light + alignment)
      alignment < 0 and light > 0 ->
        max(0, light + alignment)
      true ->
        light
    end
    
  end

  def lights(items) do
    items
    |> Enum.filter(&(Entity.has_component?(&1, Components.Effects)))
    |> Enum.map(fn(item) ->
         item
         |> Components.Effects.lights
         |> Enum.sum
       end)
    |> Enum.sum
  end

  def light_in_room(room) do
    Room.items(room)
    |> lights
  end

  def light_on_monsters(room) do
    room
    |> Systems.Room.living_in_room
    |> Enum.map(fn(monster) ->
         Systems.Limbs.equipped_items(monster) ++ Components.Items.get_items(monster)
       end)
    |> List.flatten
    |> lights
  end

  def entities(entity, room) do
    Room.monsters(room) |> Enum.reject(&(entity == &1))
  end

  def monsters_html(spirit, room) do
    monsters = Room.monsters(room)
    case Enum.count(monsters) do
      0 ->
        ""
      _ ->
        monsters = monsters
                   |> Enum.map(fn(monster) ->
                        cond do
                          Monster.evil?(monster) ->
                            "<span class='magenta'>#{Monster.name(monster)}</span>"
                          Monster.good?(monster) ->
                            "<span class='grey'>#{Monster.name(monster)}</span>"
                          Monster.neutral?(monster) ->
                            "<span class='dark-cyan'>#{Monster.name(monster)}</span>"
                        end
                      end)
                   |> Enum.join("<span class='magenta'>, </span>")
        "<div class='entities'><span class='dark-magenta'>Also here:</span> #{monsters}<span class='dark-magenta'>.</span></div>"
    end
  end

  def move(spirit, monster, direction) do
    Systems.Exit.move(spirit, monster, direction)
  end

end
