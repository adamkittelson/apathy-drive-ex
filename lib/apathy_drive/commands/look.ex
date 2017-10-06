defmodule ApathyDrive.Commands.Look do
  require Logger
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Doors, Item, Match, Mobile, RoomServer, Stealth}

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def keywords, do: ["look", "l"]

  def execute(%Room{} = room, %Character{} = character, args) do
    if blind?(character) do
      Mobile.send_scroll(character, "<p>You are blind.</p>")
    else
      look(room, character, args)
    end
    room
  end

  def look(%Room{id: id} = room, %Character{room_id: room_id} = character, []) when id != room_id do
    peek(room, character, room_id)
    look(room, Map.put(character, :room_id, id), [])
  end

  def look(%Room{} = room, %Character{} = character, []) do
    light = Room.light(room)

    if visible?(light, character) do
      Mobile.send_scroll(character, "<p><span class='cyan'>#{room.name}</span></p>")
      Mobile.send_scroll(character, "<p>    #{room.description}</p>")
      if Room.trainer?(room) do
        Mobile.send_scroll(character, "<p>\n<em>You can train skills here, \"list skills\" to see the skills available to train at this location.</em>\n\n</p>")
      end
      Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{look_items(room)}</span></p>")
      Mobile.send_scroll(character, look_mobiles(room, character))
      Mobile.send_scroll(character, "<p><span class='dark-green'>#{look_directions(room)}</span></p>")
      Mobile.send_scroll(character, "<p>#{light_desc(light)}</p>")
    else
      Mobile.send_scroll(character, "<p>#{light_desc(light)}</p>")
    end
  end

  def look(%Room{} = room, %Character{} = character, arguments) when is_list(arguments) do
    cond do
      Enum.member?(@directions, Enum.join(arguments, " ")) ->
        room_exit = Room.get_exit(room, Enum.join(arguments, " "))
        look(room, character, room_exit)
      target = Room.find_mobile_in_room(room, character, Enum.join(arguments, " ")) ->
        look_at_mobile(target, character)
      target = Room.find_item(room, Enum.join(arguments, " ")) ->
        look_at_item(character, target, room)
      true ->
        look_at_item(character, Enum.join(arguments, " "), room)
    end
  end

  def look(%Room{}, %Character{} = character, nil) do
    Mobile.send_scroll(character, "<p>There is no exit in that direction!</p>")
  end

  def look(%Room{}, %Character{} = character, %{"kind" => kind}) when kind in ["RemoteAction", "Command"] do
    Mobile.send_scroll(character, "<p>There is no exit in that direction!</p>")
  end

  def look(%Room{} = room, %Character{} = character, %{"kind" => "Hidden"} = room_exit) do
    if Doors.open?(room, room_exit) do
      look(room, character, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>There is no exit in that direction!</p>")
    end
  end

  def look(%Room{}, %Character{} = character, %{"destination" => destination}) do
    destination
    |> RoomServer.find
    |> RoomServer.look(character, [])
  end

  def peek(%Room{} = room, %Character{} = character, room_id) do
    mirror_exit = Room.mirror_exit(room, room_id)

    if mirror_exit do
      room.mobiles
      |> Map.values
      |> List.delete(character)
      |> Enum.each(fn
           %Character{} = observer ->
             message = "#{Mobile.colored_name(character, observer)} peeks in from #{Room.enter_direction(mirror_exit["direction"])}!"

             Mobile.send_scroll(observer, "<p><span class='dark-magenta'>#{message}</span></p>")
           _ ->
             :noop
         end)
    end
  end

  def look_at_mobile(%{} = target, %Character{} = character) do
    if character != target, do: Mobile.send_scroll(target, "<p>#{Mobile.colored_name(character, target)} looks you over.</p>")

    hp_description = Mobile.hp_description(target)

    hp_description =
      "{{target:He appears/She appears/They appear}} to be #{hp_description}."
      |> interpolate(%{"target" => target})

    description =
      target
      |> Mobile.description(character)
      |> interpolate(%{"target" => target})

    Mobile.send_scroll(character, "<p>#{description}</p>")
    Mobile.send_scroll(character, "<p>#{hp_description}</p>")
  end

  def look_mobiles(%Room{mobiles: mobiles} = room, character \\ nil) do
    mobiles_to_show =
      mobiles
      |> Map.values
      |> List.delete(character)
      |> Enum.filter(&Stealth.visible?(&1, character, room))
      |> Enum.map(&Mobile.colored_name(&1, character))

    if Enum.any?(mobiles_to_show) do
      "<p><span class='dark-magenta'>Also here:</span> #{Enum.join(mobiles_to_show, ", ")}<span class='dark-magenta'>.</span></p>"
    else
      ""
    end
  end

  def light_desc(light_level) when light_level < -1000, do: "<p>You are blind.</p>"
  def light_desc(light_level) when light_level <= -200, do: "<p>The room is pitch black - you can't see anything</p>"
  def light_desc(light_level) when light_level <= -150, do: "<p>The room is very dark - you can't see anything</p>"
  def light_desc(light_level) when light_level <= -100, do: "<p>The room is barely visible</p>"
  def light_desc(light_level) when light_level < 0,  do: "<p>The room is dimly lit</p>"
  def light_desc(light_level) when light_level >= 300,  do: "<p>The room is blindingly bright - you can't see anything</p>"
  def light_desc(light_level) when light_level >= 200,  do: "<p>The room is painfully bright - you can't see anything</p>"
  def light_desc(light_level) when light_level >= 100,  do: "<p>The room is dazzlingly bright</p>"
  def light_desc(light_level) when light_level >= 25,   do: "<p>The room is brightly lit</p>"
  def light_desc(_light_level), do: nil

  def visible?(light, %Character{} = _character) do
    light > -150 and light < 200
  end

  def display_direction(%{"kind" => "Hidden"} = room_exit, room) do
    if Doors.open?(room, room_exit), do: room_exit["description"]
  end
  def display_direction(%{"kind" => kind}, _room) when kind in ["Command", "RemoteAction"], do: nil
  def display_direction(%{"direction" => direction}, _room), do: direction

  def exit_directions(%Room{} = room) do
    room.exits
    |> Enum.map(fn(room_exit) ->
        display_direction(room_exit, room)
       end)
    |> Enum.reject(&(&1 == nil))
  end

  def look_directions(%Room{} = room) do
    case exit_directions(room) do
      [] ->
        "Obvious exits: NONE"
      directions ->
        "Obvious exits: #{Enum.join(directions, ", ")}"
    end
  end

  def look_items(%Room{} = room) do
    psuedo_items = room.item_descriptions["visible"]
                   |> Map.keys

    items = Enum.map(room.items, &Item.colored_name(&1))

    items = items ++ psuedo_items

    case Enum.count(items) do
      0 ->
        ""
      _ ->
        "You notice #{Enum.join(items, ", ")} here."
    end
  end

  def blind?(%Character{} = character) do
    character.effects
    |> Map.values
    |> Enum.any?(&(Map.has_key?(&1, "blinded")))
  end

  def look_at_item(%Character{} = character, %Item{} = item, room) do
    current = Character.score_data(character, room)
    case ApathyDrive.Commands.Wear.equip_item(character, item, false) do
      false ->
        Mobile.send_scroll(character, "<p>#{item.description}</p>")
        item.effects
        |> Map.values
        |> Enum.filter(&(Map.has_key?(&1, "StatusMessage")))
        |> Enum.map(&(&1["StatusMessage"]))
        |> Enum.each(fn(effect_message) ->
          Mobile.send_scroll(character, "<p>#{effect_message}</p>")
        end)
        Mobile.send_scroll(character, "\n\n<p><span class='dark-green'>Worn On:</span> <span class='dark-cyan'>#{item.worn_on}</span> <span class='dark-green'>Skill:</span> <span class='dark-cyan'>#{item.grade}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{item.level}</span></p>")
      %{equipped: _, character: equipped} ->
        Mobile.send_scroll(character, "<p>#{item.description}</p>")
        item.effects
        |> Map.values
        |> Enum.filter(&(Map.has_key?(&1, "StatusMessage")))
        |> Enum.map(&(&1["StatusMessage"]))
        |> Enum.each(fn(effect_message) ->
          Mobile.send_scroll(character, "<p>#{effect_message}</p>")
        end)
        Mobile.send_scroll(character, "\n\n<p><span class='dark-green'>Worn On:</span> <span class='dark-cyan'>#{item.worn_on}</span> <span class='dark-green'>Skill:</span> <span class='dark-cyan'>#{item.grade}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{item.level}</span></p>")

        equipped = Character.score_data(equipped, room)

        score_data =
          current
          |> Enum.reduce(%{}, fn({key, val}, values) ->
               Map.put(values, key, %{color: color(val, equipped[key]), value: equipped[key]})
             end)

        hits = Enum.join([score_data.hp.value, trunc(score_data.max_hp.value)], "/") |> String.pad_trailing(13)
        mana = Enum.join([score_data.mana.value, trunc(score_data.max_mana.value)], "/") |> String.pad_trailing(13)

        Mobile.send_scroll(character, "\n\n<p><span class='dark-yellow'>Changes if Equipped:</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Name:</span> <span class='#{score_data.name.color}'>#{String.pad_trailing(score_data.name.value, 13)}</span><span class='dark-green'>Level:</span> <span class='#{score_data.level.color}'>#{String.pad_trailing(to_string(score_data.level.value), 13)}</span><span class='dark-green'>Exp:</span> <span class='#{score_data.experience.color}'>#{String.pad_leading(to_string(score_data.experience.value), 13)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Race:</span> <span class='#{score_data.race.color}'>#{String.pad_trailing(score_data.race.value, 13)}</span><span class='dark-green'>Melee DPS:</span> <span class='#{score_data.melee_dps.color}'>#{String.pad_trailing(to_string(Float.round(score_data.melee_dps.value, 2)), 9)}</span><span class='dark-green'>Perception:</span> <span class='#{score_data.perception.color}'>#{String.pad_leading(to_string(trunc(score_data.perception.value)), 6)}</span></p>")
        Mobile.send_scroll(character, "<p>                   <span class='dark-green'>Physical Res:</span> <span class='#{score_data.physical_resistance.color}'>#{String.pad_trailing(to_string(trunc(score_data.physical_resistance.value)), 6)}</span><span class='dark-green'>Stealth:</span> <span class='#{score_data.stealth.color}'>#{String.pad_leading(to_string(trunc(score_data.stealth.value)), 9)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Hits:</span> <span class='#{score_data.max_hp.color}'>#{hits}</span><span class='dark-green'>Magical Dmg:</span> <span class='#{score_data.magical_damage.color}'>#{String.pad_trailing(to_string(trunc(score_data.magical_damage.value)), 7)}</span><span class='dark-green'>Tracking:</span> <span class='#{score_data.tracking.color}'>#{String.pad_leading(to_string(trunc(score_data.tracking.value)), 8)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Mana:</span> <span class='#{score_data.max_mana.color}'>#{mana}</span><span class='dark-green'>Magical Res:</span> <span class='#{score_data.magical_resistance.color}'>#{String.pad_trailing(to_string(trunc(score_data.magical_resistance.value)), 7)}</span><span class='dark-green'>Accuracy:</span> <span class='#{score_data.accuracy.color}'>#{String.pad_leading(to_string(trunc(score_data.accuracy.value)), 8)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>#{String.pad_leading("Dodge:", 45)}</span> <span class='#{score_data.dodge.color}'>#{String.pad_leading(to_string(trunc(score_data.dodge.value)), 11)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Strength:</span>  <span class='#{score_data.strength.color}'>#{String.pad_trailing(to_string(trunc(score_data.strength.value)), 7)}</span> <span class='dark-green'>Agility:</span> <span class='#{score_data.agility.color}'>#{String.pad_trailing(to_string(trunc(score_data.agility.value)), 11)}</span><span class='dark-green'>Spellcasting:</span> <span class='#{score_data.spellcasting.color}'>#{String.pad_leading(to_string(trunc(score_data.spellcasting.value)), 4)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Intellect:</span> <span class='#{score_data.intellect.color}'>#{String.pad_trailing(to_string(trunc(score_data.intellect.value)), 7)}</span> <span class='dark-green'>Health:</span>  <span class='#{score_data.health.color}'>#{String.pad_trailing(to_string(trunc(score_data.health.value)), 11)}</span><span class='dark-green'>Crits:</span> <span class='#{score_data.crits.color}'>#{String.pad_leading(to_string(trunc(score_data.crits.value)), 11)}</span></p>")
        Mobile.send_scroll(character, "<p><span class='dark-green'>Willpower:</span> <span class='#{score_data.willpower.color}'>#{String.pad_trailing(to_string(trunc(score_data.willpower.value)), 7)}</span> <span class='dark-green'>Charm:</span>   <span class='#{score_data.charm.color}'>#{String.pad_trailing(to_string(trunc(score_data.charm.value)), 11)}</span><span class='dark-green'>Item Level:</span> <span class='#{score_data.item_level.color}'>#{String.pad_leading(to_string(trunc(score_data.item_level.value)), 6)}</span></p>")
    end
  end

  def look_at_item(mobile, %{description: description}, _room) do
    Mobile.send_scroll mobile, "<p>#{description}</p>"
  end

  def look_at_item(%Character{} = mobile, item_name, room) do
    case find_item(mobile, item_name) do
      nil ->
        Mobile.send_scroll(mobile, "<p>You do not notice that here.</p>")
      item ->
        look_at_item(mobile, item, room)
    end
  end

  defp color(pre, post) when pre > post do
    "dark-red"
  end
  defp color(pre, post) when pre < post do
    "green"
  end
  defp color(_pre, _post) do
    "dark-cyan"
  end

  defp find_item(%Character{inventory: items}, item) do
    item =
      items
      |> Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        nil
      %Item{} = item ->
        item
    end
  end


end
