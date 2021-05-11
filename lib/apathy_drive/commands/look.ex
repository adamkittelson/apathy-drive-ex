defmodule ApathyDrive.Commands.Look do
  require Logger
  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    Character,
    Commands.Inventory,
    Currency,
    Doors,
    Regeneration,
    Item,
    ItemType,
    Match,
    Mobile,
    Repo,
    RoomServer,
    Shop,
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

  def execute(%Room{} = room, %Character{} = character, ignore_light: true) do
    coords =
      if character.admin do
        "<span style='font-size:0.7em'>Room##{room.id} (x:#{room.coordinates["x"]} y:#{
          room.coordinates["y"]
        } z:#{room.coordinates["z"]})</span>"
      else
        ""
      end

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
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    if blind?(character) do
      Mobile.send_scroll(character, "<p>You are blind.</p>")
    else
      look(room, character, args)
    end

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
        "<span style='font-size:0.7em'>Room##{room.id} (x:#{room.coordinates["x"]} y:#{
          room.coordinates["y"]
        } z:#{room.coordinates["z"]})</span>"
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
        target = Map.put(target, :level, target.level || character.level)
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

  def look_at_mobile(%Character{} = target, %{} = character) do
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

    if target.equipment |> Enum.any?() do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-yellow'>{{target:He/She/They}} is equipped with:</span></p>"
        |> interpolate(%{"target" => target})
      )

      target.equipment
      |> Enum.sort_by(fn item ->
        Enum.find_index(ApathyDrive.Commands.Inventory.slot_order(), &(&1 == item.worn_on))
      end)
      |> Enum.each(fn item ->
        worn_on =
          if item.type == "Light" do
            String.pad_trailing("(Readied/#{item.uses})", 15)
          else
            String.pad_trailing("(#{item.worn_on})", 15)
          end

        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'>#{worn_on}</span><span class='dark-green'>#{
            Item.colored_name(item, character: target)
          }</span></p>"
        )
      end)

      Mobile.send_scroll(character, "<br>")
    end
  end

  def look_at_mobile(%{} = target, %{} = character) do
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

  def light_desc(light_level) when light_level > 25, do: "<p>The room is brightly lit</p>"
  def light_desc(_light_level), do: nil

  def visible?(light) do
    light >= -150
  end

  def light_for_character(light, character) do
    dark_vision = Mobile.ability_value(character, "DarkVision")

    if light < 0 do
      min(0, light + dark_vision)
    else
      max(0, light)
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
      |> Enum.reject(& &1.hidden)
      |> Enum.map(&Item.colored_name(&1, character: character))

    items = Currency.to_list(room) ++ items

    case Enum.count(items) do
      0 ->
        ""

      _ ->
        "You notice #{Inventory.to_sentence(items)} here."
    end
  end

  def blind?(%Character{} = character) do
    Mobile.has_ability?(character, "Blind")
  end

  def weapon_damage(%Character{} = character, ability \\ nil) do
    ability = ability || Mobile.attack_ability(character)

    attack_interval = Regeneration.duration_for_energy(character, max(ability.energy, 200))

    {min_damage, max_damage} =
      Enum.reduce(ability.traits["Damage"], {0, 0}, fn damage, {min_damage, max_damage} ->
        {min_damage + damage.min, max_damage + damage.max}
      end)

    average = (min_damage + max_damage) / 2

    dps = Float.round(average / (attack_interval / 1000), 2)

    %{dps: dps, min_damage: min_damage, max_damage: max_damage, ability: ability}
  end

  def affix_trait_descriptions(item, character) do
    item.affix_traits
    |> Enum.reject(&is_nil(&1.description))
    |> Enum.map(fn instance_affix_trait ->
      affix_description(
        character,
        instance_affix_trait.affix_trait.trait.name,
        instance_affix_trait.description,
        instance_affix_trait.value
      )
    end)
    |> Enum.join("\n")
  end

  def affix_description(character, "DefensePerLevel", description, val) do
    ApathyDrive.Text.interpolate(description, %{"amount" => val * character.level})
  end

  def affix_description(_character, _trait, description, _val), do: description

  def defense(item, character) do
    value =
      Systems.Effect.effect_bonus(item, "Defense") +
        Systems.Effect.effect_bonus(item, "DefensePerLevel") * character.level

    cond do
      value <= 0 ->
        ""

      value > item.ac ->
        "\nDefense: <span style='color: #4850B8'>#{value}</span>"

      item.quality == "low" ->
        "\nDefense: <span class='dark-red'>#{value}</span>"

      :else ->
        "\nDefense: #{value}"
    end
  end

  def damage(%{min_damage: nil, max_damage: nil}, _character), do: ""

  def damage(%Item{min_damage: min, max_damage: max} = item, character) do
    modifier =
      (100 + (Systems.Effect.effect_bonus(item, "Damage%") || 0) +
         Character.mastery_value(character, item, "Damage%")) / 100

    {min_dam, max_dam} =
      if modifier > 1 do
        {max(trunc(min * modifier), min + 1), max(trunc(max * modifier), max + 1)}
      else
        {min, max}
      end

    min_dam = min_dam + (Systems.Effect.effect_bonus(item, "MinDamage") || 0)

    max_dam =
      max_dam + (Systems.Effect.effect_bonus(item, "MaxDamage") || 0) +
        (Systems.Effect.effect_bonus(item, "MaxDamagePerLevel") || 0) * character.level

    {min_dam, max_dam} =
      Enum.reduce(
        ["ColdDamage", "ElectricityDamage", "FireDamage"],
        {min_dam, max_dam},
        fn damage, {min_dam, max_dam} ->
          list = Systems.Effect.effect_bonus(item, damage)

          Enum.reduce(list, {min_dam, max_dam}, fn %{"max" => max, "min" => min},
                                                   {min_dam, max_dam} ->
            {min_dam + min, max_dam + max}
          end)
        end
      )

    min_dam =
      if min_dam > min do
        "<span style='color: #4850B8'>#{min_dam}</span>"
      else
        min
      end

    max_dam =
      if max_dam > max do
        "<span style='color: #4850B8'>#{max_dam}</span>"
      else
        max
      end

    "\nDamage: #{min_dam}-#{max_dam}"
  end

  def block_chance(%Item{block_chance: nil}, _character), do: ""

  def block_chance(%Item{block_chance: chance} = item, character) do
    value = Character.block_chance(character, item)

    cond do
      value <= 0 ->
        ""

      value >= 75 ->
        "\nChance to Block: <span style='color: #908858'>#{value}%</span>"

      value > chance ->
        "\nChance to Block: <span style='color: #4850B8'>#{value}%</span>"

      :else ->
        "\nChance to Block: #{value}%"
    end
  end

  def required_strength(character, %Item{} = item) do
    if (strength = Item.required_strength(item)) > 0 do
      if Mobile.attribute_at_level(character, :strength, character.level) >= strength do
        "\nRequired Strength: #{strength}"
      else
        "\n<span class='red'>Required Strength: #{strength}</span>"
      end
    else
      ""
    end
  end

  def required_agility(character, %Item{} = item) do
    if (agility = Item.required_agility(item)) > 0 do
      if Mobile.attribute_at_level(character, :agility, character.level) >= agility do
        "\nRequired Agility: #{agility}"
      else
        "\n<span class='red'>Required Agility: #{agility}</span>"
      end
    else
      ""
    end
  end

  def required_level(character, %Item{} = item) do
    if (level = Item.required_level(item)) > 0 do
      if character.level >= level do
        "\nRequired Level: #{level}"
      else
        "\n<span class='red'>Required Level: #{level}</span>"
      end
    else
      ""
    end
  end

  def weapon_speed(%Item{speed: nil}), do: ""

  def weapon_speed(%Item{speed: _speed, type_id: nil} = _item), do: ""

  def weapon_speed(%Item{speed: speed, type_id: type_id} = item) do
    type = Repo.get(ItemType, type_id).name
    desc = weapon_speeds(speed)

    ias = Systems.Effect.effect_bonus(item, "IncreasedAttackSpeed") || 0

    ias_multiplier = (100 - ias) / 100

    ias_energy = trunc(speed * ias_multiplier)

    enhanced_desc = weapon_speeds(ias_energy)

    if enhanced_desc != desc do
      "\n#{type} Class: <span style='color: #908858'>#{enhanced_desc} Attack Speed</span>"
    else
      "\n#{type} Class: #{enhanced_desc} Attack Speed"
    end
  end

  def weapon_speeds(energy) when energy > 800, do: "Very Slow"
  def weapon_speeds(energy) when energy > 500 and energy <= 800, do: "Slow"
  def weapon_speeds(energy) when energy > 330 and energy <= 500, do: "Normal"
  def weapon_speeds(energy) when energy > 250 and energy <= 330, do: "Fast"
  def weapon_speeds(energy) when energy <= 250, do: "Very Fast"

  def item_tooltip(%Character{} = character, %Item{} = item) do
    value =
      %Shop{cost_multiplier: 1}
      |> Shop.sell_price(character, item)
      |> Currency.set_value()
      |> Currency.to_string()
      |> case do
        "" -> "FREE"
        value -> value
      end

    """
      <span>#{Item.colored_name(item, titleize: true, no_tooltip: true)}</span>#{
      block_chance(item, character)
    }#{damage(item, character)}#{defense(item, character)}#{required_level(character, item)}#{
      required_strength(character, item)
    }#{required_agility(character, item)}#{weapon_speed(item)}
      <span style='color: #4850B8'>#{affix_trait_descriptions(item, character)}</span>

      Sells For: #{value}
    """
  end

  def item_tooltip(%Character{} = _character, _item), do: ""

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

  def look_at_item(%Character{} = character, %Item{worn_on: nil} = item) do
    value =
      %Shop{cost_multiplier: 1}
      |> Shop.sell_price(character, item)
      |> Currency.set_value()
      |> Currency.to_string()
      |> case do
        "" -> "FREE"
        value -> value
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

    if item.worn_on do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Type:</span> <span class='dark-cyan'>#{item.armour_type}</span></p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Worn On:</span> <span class='dark-cyan'>#{item.worn_on}</span> </p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Weight:</span> <span class='dark-cyan'>#{item.weight}</span></p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Value:</span> <span class='dark-cyan'>#{value}</span></p>"
      )
    end

    display_traits(character, item)

    display_enchantment(character, item)
  end

  def look_at_item(%Character{} = character, %Item{} = item) do
    item = item_tooltip(character, item)
    Mobile.send_scroll(character, "<p class='item'>#{item}</span>")
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

  def display_traits(character, item, indent \\ 0) do
    traits =
      item.effects
      |> Map.values()
      |> Enum.reduce(%{}, &Trait.merge_traits(&2, &1))
      |> Ability.process_duration_traits(character, character, nil)
      |> Map.put_new("Defense", 0)
      |> Map.put_new("MR", 0)

    Enum.each(traits, &display_trait(character, item, &1, indent))
  end

  def display_trait(_character, _item, {"Defense", 0}, _indent), do: :noop
  def display_trait(_character, _item, {"MR", 0}, _indent), do: :noop
  def display_trait(_character, _item, {"RemoveMessage", _msg}, _indent), do: :noop
  def display_trait(_character, _item, {"stack_count", _msg}, _indent), do: :noop
  def display_trait(_character, _item, {"stack_key", _msg}, _indent), do: :noop
  def display_trait(_character, _item, {"Enchantment", _msg}, _indent), do: :noop

  def display_trait(_character, _item, {"Learn", _list}, _indent), do: :noop

  def display_trait(character, _item, {"Magical", list}, indent) when is_list(list) do
    if Enum.any?(list) do
      Mobile.send_scroll(
        character,
        "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Magical:</span> <span class='dark-cyan'>true</span></p>"
      )
    end
  end

  def display_trait(_character, _item, {"WeaponDamage", _damage}, _indent), do: :noop
  def display_trait(_character, _item, {"OnHit%", _abilities}, _indent), do: :noop
  def display_trait(_character, _item, {"StackCount", _damage}, _indent), do: :noop
  def display_trait(_character, _item, {"StackKey", _damage}, _indent), do: :noop
  def display_trait(_character, _item, {"Del@Maint", _damage}, _indent), do: :noop

  def display_trait(character, item, {"OnHit", abilities}, indent) do
    on_hit = Systems.Effect.effect_bonus(item, "OnHit%")

    names =
      abilities
      |> Enum.map(& &1.name)
      |> ApathyDrive.Commands.Inventory.to_sentence()

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{on_hit || "100"}% chance of:</span> <span class='dark-cyan'>#{
        names
      }</span></p>"
    )
  end

  def display_trait(character, _item, {"Grant", abilities}, indent) do
    names =
      abilities
      |> Enum.map(& &1.name)
      |> ApathyDrive.Commands.Inventory.to_sentence()

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Grants the ability to cast:</span> <span class='dark-cyan'>#{
        names
      }</span></p>"
    )
  end

  def display_trait(character, _item, {"Damage", list}, indent) when is_list(list) do
    Enum.each(list, fn %{kind: kind, damage_type: type, min: min, max: max} ->
      Mobile.send_scroll(
        character,
        "<p>#{String.pad_trailing("", indent)}<span class='dark-cyan'>#{min}-#{max} #{kind} #{
          String.downcase(type)
        } damage</span></p>"
      )
    end)
  end

  def display_trait(character, _item, {"Defense%", value}, indent) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(value)

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>AC:</span> <span class='dark-cyan'>#{
        ac_from_percent
      }</span></p>"
    )
  end

  def display_trait(character, item, {"Powerstone", _}, indent) do
    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Powerstone:</span> <span class='dark-cyan'>#{
        trunc(item.uses)
      }/#{item.max_uses} mana</span></p>"
    )
  end

  def display_trait(character, _item, {"MR%", value}, indent) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(value)

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>MR:</span> <span class='dark-cyan'>#{
        ac_from_percent
      }</span></p>"
    )
  end

  def display_trait(character, _item, {"RemoveSpells", list}, indent) do
    list =
      list
      |> Enum.map(&Ability.find/1)
      |> Enum.map(& &1.name)

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Remove Spells:</span> <span class='dark-cyan'>#{
        inspect(list)
      }</span></p>"
    )
  end

  def display_trait(_character, _item, {"ClassOk", _list}, _indent), do: :noop
  # def display_trait(character, _item, {"ClassOk", list}, indent) do
  #   list =
  #     list
  #     |> List.wrap()
  #     |> Enum.map(&ApathyDrive.Repo.get!(ApathyDrive.Class, &1))
  #     |> Enum.map(& &1.name)
  #     |> ApathyDrive.Commands.Inventory.to_sentence()

  #   Mobile.send_scroll(
  #     character,
  #     "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>ClassOk:</span> <span class='dark-cyan'>#{
  #       list
  #     }</span></p>"
  #   )
  # end

  def display_trait(character, _item, {"Claimed", id}, indent) do
    if owner = Repo.get(Character, id) do
      Mobile.send_scroll(
        character,
        "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Claimed by:</span> <span class='dark-cyan'>#{
          owner.name
        }</span></p>"
      )
    end
  end

  def display_trait(_character, _item, {"timers", _list}, _indent), do: :noop
  def display_trait(_character, _item, {"effect_ref", _list}, _indent), do: :noop

  def display_trait(character, _item, {trait, list}, indent) when is_list(list) do
    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{trait}:</span> <span class='dark-cyan'>#{
        inspect(list)
      }</span></p>"
    )
  end

  def display_trait(_character, _item, {_trait, %{}}, _indent), do: :noop

  def display_trait(character, _item, {trait, value}, indent) do
    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{trait}:</span> <span class='dark-cyan'>#{
        inspect(value)
      }</span></p>"
    )
  end

  def display_enchantment(_character, %Item{type: "Stone"}), do: :noop

  def display_enchantment(character, %Item{enchantments: list}) when length(list) > 0 do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Enchantments:</span> <span class='dark-cyan'>#{
        ApathyDrive.Commands.Inventory.to_sentence(list)
      }</span></p>"
    )
  end

  def display_enchantment(_character, %Item{}), do: :noop

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
