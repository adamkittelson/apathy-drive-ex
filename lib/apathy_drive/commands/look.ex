defmodule ApathyDrive.Commands.Look do
  require Logger
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    Commands.Inventory,
    Currency,
    Doors,
    Regeneration,
    Item,
    Match,
    Mobile,
    RoomServer,
    Trait
  }

  @directions [
    "n",
    "north",
    "ne",
    "northeast",
    "e",
    "east",
    "se",
    "southeast",
    "s",
    "south",
    "sw",
    "southwest",
    "w",
    "west",
    "nw",
    "northwest",
    "u",
    "up",
    "d",
    "down"
  ]

  def keywords, do: ["look", "l"]

  def execute(%Room{} = room, %Character{} = character, args) do
    if blind?(character) do
      Mobile.send_scroll(character, "<p>You are blind.</p>")
    else
      look(room, character, args)
    end

    Character.update_score(character, room)
    room
  end

  def look(%Room{id: id} = room, %Character{room_id: room_id} = character, [])
      when id != room_id do
    peek(room, character, room_id)
    look(room, Map.put(character, :room_id, id), [])
  end

  def look(%Room{} = room, %Character{} = character, []) do
    light =
      room
      |> Room.light()
      |> light_for_character(character)

    coords =
      if character.admin do
        "(x:#{room.coordinates["x"]} y:#{room.coordinates["y"]} z:#{room.coordinates["z"]})"
      else
        ""
      end

    if visible?(light) do
      Mobile.send_scroll(character, "<p><span class='cyan'>#{room.name}</span> #{coords}</p>")
      Mobile.send_scroll(character, "<p>    #{room.description}</p>")

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>#{look_items(room, character)}</span></p>"
      )

      Mobile.send_scroll(character, look_mobiles(room, character))

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>#{look_directions(room)}</span></p>"
      )

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
        look_at_item(character, target)

      true ->
        look_at_item(character, Enum.join(arguments, " "))
    end
  end

  def look(%Room{}, %Character{} = character, nil) do
    Mobile.send_scroll(character, "<p>There is no exit in that direction!</p>")
  end

  def look(%Room{}, %Character{} = character, %{"kind" => kind})
      when kind in ["RemoteAction", "Command"] do
    Mobile.send_scroll(character, "<p>There is no exit in that direction!</p>")
  end

  def look(%Room{} = room, %Character{} = character, %{"kind" => "Door"} = room_exit) do
    if Doors.open?(room, room_exit) do
      look(room, character, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>The door is closed in that direction!</p>")
    end
  end

  def look(%Room{} = room, %Character{} = character, %{"kind" => "Key"} = room_exit) do
    if Doors.open?(room, room_exit) do
      look(room, character, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>The door is closed in that direction!</p>")
    end
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
    |> RoomServer.find()
    |> RoomServer.look(character, [])
  end

  def peek(%Room{} = room, %Character{} = character, room_id) do
    mirror_exit = Room.mirror_exit(room, room_id)

    if mirror_exit do
      room.mobiles
      |> Map.values()
      |> List.delete(character)
      |> Enum.each(fn
        %Character{} = observer ->
          message =
            "#{Mobile.colored_name(character)} peeks in from #{
              Room.enter_direction(mirror_exit["direction"])
            }!"

          Mobile.send_scroll(observer, "<p><span class='dark-magenta'>#{message}</span></p>")

        _ ->
          :noop
      end)
    end
  end

  def look_at_mobile(%{} = target, %Character{} = character) do
    if character != target,
      do:
        Mobile.send_scroll(
          target,
          "<p>#{Mobile.colored_name(character)} looks you over.</p>"
        )

    hp_description = Mobile.hp_description(target)

    hp_description =
      "{{target:He appears/She appears/They appear}} to be #{hp_description}."
      |> interpolate(%{"target" => target})

    description =
      target
      |> Mobile.description(character)
      |> interpolate(%{"target" => target})

    Mobile.send_scroll(character, "<p>#{description}</p>")
    Mobile.send_scroll(character, "<p>#{hp_description}\n\n</p>")

    ApathyDrive.DamageType
    |> ApathyDrive.Repo.all()
    |> Enum.map(&{&1.name, Mobile.ability_value(target, "Resist#{&1.name}")})
    |> Enum.reject(&(elem(&1, 1) == 0))
    |> Enum.chunk_every(4)
    |> Enum.each(fn chunks ->
      chunks =
        Enum.map(chunks, fn {name, value} ->
          "<span class='dark-green'>#{String.pad_trailing("Resist" <> name <> ":", 16)}</span> <span class='dark-cyan'>#{
            String.pad_leading(to_string(value), 3)
          }</span>"
        end)

      Mobile.send_scroll(character, "<p>" <> Enum.join(chunks, "    ") <> "</p>")
    end)
  end

  def look_mobiles(%Room{mobiles: mobiles}, character \\ nil) do
    mobiles_to_show =
      mobiles
      |> Map.values()
      |> List.delete(character)
      |> Enum.filter(&(!&1.sneaking))
      |> Enum.map(&Mobile.colored_name(&1))

    if Enum.any?(mobiles_to_show) do
      "<p><span class='dark-magenta'>Also here:</span> #{Enum.join(mobiles_to_show, ", ")}<span class='dark-magenta'>.</span></p>"
    else
      ""
    end
  end

  def light_desc(light_level) when light_level < -1000, do: "<p>You are blind.</p>"

  def light_desc(light_level) when light_level < -200,
    do: "<p>The room is pitch black - you can't see anything</p>"

  def light_desc(light_level) when light_level < -150,
    do: "<p>The room is very dark - you can't see anything</p>"

  def light_desc(light_level) when light_level < -100, do: "<p>The room is barely visible</p>"
  def light_desc(light_level) when light_level < 0, do: "<p>The room is dimly lit</p>"

  def light_desc(light_level) when light_level > 300,
    do: "<p>The room is blindingly bright - you can't see anything</p>"

  def light_desc(light_level) when light_level > 200,
    do: "<p>The room is painfully bright - you can't see anything</p>"

  def light_desc(light_level) when light_level > 100, do: "<p>The room is dazzlingly bright</p>"
  def light_desc(light_level) when light_level > 25, do: "<p>The room is brightly lit</p>"
  def light_desc(_light_level), do: nil

  def visible?(light) do
    light >= -150 and light < 200
  end

  def light_for_character(light, character) do
    dark_vision = Mobile.ability_value(character, "DarkVision")
    light_vision = Mobile.ability_value(character, "LightVision")

    if light < 0 do
      min(0, light + dark_vision)
    else
      max(0, light - light_vision)
    end
  end

  def display_direction(%{"kind" => "Gate", "direction" => direction} = room_exit, room) do
    if Doors.open?(room, room_exit),
      do: "open gate #{direction}",
      else: "closed gate #{direction}"
  end

  def display_direction(%{"kind" => "Door", "direction" => direction} = room_exit, room) do
    if Doors.open?(room, room_exit),
      do: "open door #{direction}",
      else: "closed door #{direction}"
  end

  def display_direction(%{"kind" => "Key", "direction" => direction} = room_exit, room) do
    if Doors.open?(room, room_exit),
      do: "open door #{direction}",
      else: "closed door #{direction}"
  end

  def display_direction(%{"kind" => "Hidden"} = room_exit, room) do
    if Doors.open?(room, room_exit), do: room_exit["description"]
  end

  def display_direction(%{"kind" => kind}, _room) when kind in ["Command", "RemoteAction"],
    do: nil

  def display_direction(%{"direction" => direction}, _room), do: direction

  def exit_directions(%Room{} = room) do
    room.exits
    |> Enum.map(fn room_exit ->
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

  def look_items(%Room{} = room, %Character{} = character) do
    items =
      room.items
      |> Enum.filter(
        &(&1.dropped_for_character_id == character.id or is_nil(&1.dropped_for_character_id))
      )
      |> Enum.reject(& &1.hidden)
      |> Enum.map(&Item.colored_name(&1))

    items = Currency.to_list(room) ++ items

    case Enum.count(items) do
      0 ->
        ""

      _ ->
        "You notice #{Inventory.to_sentence(items)} here."
    end
  end

  def blind?(%Character{} = character) do
    character.effects
    |> Map.values()
    |> Enum.any?(&Map.has_key?(&1, "blinded"))
  end

  def look_at_item(%Character{} = character, %Item{type: "Weapon"} = item) do
    ability = Character.ability_for_weapon(character, item)

    attack_interval = Regeneration.duration_for_energy(character, ability.energy)

    {min_damage, max_damage} =
      Enum.reduce(ability.traits["Damage"], {0, 0}, fn damage, {min_damage, max_damage} ->
        {min_damage + damage.min, max_damage + damage.max}
      end)

    average = (min_damage + max_damage) / 2

    dps = Float.round(average / (attack_interval / 1000), 2)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Name:</span> " <>
        "<span class='dark-cyan'>#{item.name}</span> " <>
        "<span class='dark-green'>Kind:</span> " <>
        "<span class='dark-cyan'>#{item.weapon_type}</span> " <>
        "<span class='dark-green'>DPS:</span> " <>
        "<span class='dark-cyan'>#{dps} (#{trunc(min_damage)}-#{trunc(max_damage)} @ #{
          trunc(ability.energy / 10)
        }% energy per swing)</span></p>"
    )

    Mobile.send_scroll(character, "<p>#{item.description}</p>")

    item.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "StatusMessage"))
    |> Enum.map(& &1["StatusMessage"])
    |> Enum.each(fn effect_message ->
      Mobile.send_scroll(character, "<p>#{effect_message}</p>")
    end)

    display_traits(character, item.traits)

    display_enchantment(character, item)
  end

  def look_at_item(%Character{} = character, %Item{type: "Sign"} = item) do
    Mobile.send_scroll(
      character,
      "<p>#{item.description}</p>"
    )
  end

  def look_at_item(%Character{} = character, %Item{getable: false, hidden: true} = item) do
    Mobile.send_scroll(
      character,
      "<p>#{item.description}</p>"
    )
  end

  def look_at_item(%Character{} = character, %Item{} = item) do
    if item.worn_on do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{item.name}</span> <span class='dark-green'>Type:</span> <span class='dark-cyan'>#{
          item.armour_type
        }</span> <span class='dark-green'>Worn On:</span> <span class='dark-cyan'>#{item.worn_on}</span></p>"
      )
    else
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{item.name}</span></p>"
      )
    end

    Mobile.send_scroll(
      character,
      "<p>#{item.description}</p>"
    )

    item.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "StatusMessage"))
    |> Enum.map(& &1["StatusMessage"])
    |> Enum.each(fn effect_message ->
      Mobile.send_scroll(character, "<p>#{effect_message}</p>")
    end)

    display_traits(character, item.traits)

    display_enchantment(character, item)
  end

  def look_at_item(mobile, %{description: description}) do
    Mobile.send_scroll(mobile, "<p>#{description}</p>")
  end

  def look_at_item(%Character{} = mobile, item_name) do
    case find_item(mobile, item_name) do
      nil ->
        Mobile.send_scroll(mobile, "<p>You do not notice that here.</p>")

      item ->
        look_at_item(mobile, item)
    end
  end

  def display_traits(character, traits) do
    traits
    |> Enum.each(&display_trait(character, &1))
  end

  def display_trait(_character, {"Learn", _list}), do: :noop

  def display_trait(character, {"Magical", list}) when is_list(list) do
    if Enum.any?(list) do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Magical:</span> <span class='dark-cyan'>true</span></p>"
      )
    end
  end

  def display_trait(character, {"OnHit", abilities}) do
    names =
      abilities
      |> Enum.map(& &1.name)
      |> ApathyDrive.Commands.Inventory.to_sentence()

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>OnHit:</span> <span class='dark-cyan'>#{names}</span></p>"
    )
  end

  def display_trait(character, {trait, list}) when is_list(list) do
    value = Trait.value(trait, list)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>#{trait}:</span> <span class='dark-cyan'>#{value}</span></p>"
    )
  end

  def display_trait(_character, {_trait, %{}}), do: :noop

  def display_trait(character, {trait, value}) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>#{trait}:</span> <span class='dark-cyan'>#{value}</span></p>"
    )
  end

  def display_enchantment(character, %Item{traits: %{"Learn" => ability}}) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>This scroll will allow you a single use of following ability:</span></p>"
    )

    ApathyDrive.Commands.Help.help(character, ability)
  end

  def display_enchantment(character, %Item{traits: %{"Passive" => ability}}) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>Equipping this item will grant you the benefits of the following ability:</span></p>"
    )

    ApathyDrive.Commands.Help.help(character, ability)
  end

  def display_enchantment(character, %Item{traits: %{"OnHit" => %{kind: "curse"} = ability}}) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>Striking an enemy with this weapon will afflict your target with the following ability:</span></p>"
    )

    ApathyDrive.Commands.Help.help(character, ability)
  end

  def display_enchantment(character, %Item{traits: %{"Grant" => ability}}) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>Equipping this item will grant you the usage of the following ability:</span></p>"
    )

    ApathyDrive.Commands.Help.help(character, ability)
  end

  def display_enchantment(_character, %Item{} = _item), do: :noop

  defp find_item(%Character{inventory: items, equipment: equipment}, item) do
    item =
      (items ++ equipment)
      |> Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        nil

      %{} = item ->
        item
    end
  end
end
