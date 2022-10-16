defmodule ApathyDrive.Commands.Look do
  require Logger
  require Ecto.Query

  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    Character,
    Commands.Inventory,
    Currency,
    Doors,
    Regeneration,
    Item,
    Level,
    Match,
    Mobile,
    Repo,
    RoomServer,
    Shop,
    Socket,
    Trainer,
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
        "<span style='font-size:0.7em'>Room##{room.id} (x:#{room.coordinates["x"]} y:#{room.coordinates["y"]} z:#{room.coordinates["z"]})</span>"
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
        "<span style='font-size:0.7em'>Room##{room.id} (x:#{room.coordinates["x"]} y:#{room.coordinates["y"]} z:#{room.coordinates["z"]})</span>"
      else
        ""
      end

    if visible?(light) do
      Mobile.send_scroll(character, "<p><span class='cyan'>#{room.name}</span> #{coords}</p>")
      Mobile.send_scroll(character, "<p>    #{room.description}</p>")

      trainables =
        cond do
          Trainer.skill_trainer?(room) and Trainer.ability_trainer?(room) ->
            "skills and abilities"

          Trainer.skill_trainer?(room) ->
            "skills"

          Trainer.ability_trainer?(room) ->
            "abilities"

          :else ->
            nil
        end

      if trainables do
        if character.class_id == room.class_id do
          guild = Trainer.guild_name(room)

          modifier = (100 + character.race.race.exp_modifier) / 100
          level = character.level
          exp = trunc(character.experience)
          tolevel = Level.exp_at_level(level, modifier)
          remaining = tolevel - exp

          level_text =
            if remaining <= 0 and character.class_id == room.class_id do
              cost =
                character
                |> Trainer.training_cost()
                |> Currency.set_value()
                |> Currency.to_string()

              "\n\nYou may <span class='green'>train</span> the #{guild} class to level #{character.level + 1} here for #{cost}."
            else
              ""
            end

          Mobile.send_scroll(
            character,
            "<p style='font-style: italic;'>\nYou may <span class='green'>list</span> and <span class='green'>train</span> #{guild} #{trainables} here. #{level_text}</span>\n\n</p>"
          )
        else
          guild = Trainer.guild_name(room)

          Mobile.send_scroll(
            character,
            "<p style='font-style: italic;'>\nYou may <span class='green'>join</span> the #{guild}'s guild here.\n\n</p>"
          )
        end
      end

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
        look_at_mobile(target, character, room)

      target = Room.find_item(room, character, Enum.join(arguments, " ")) ->
        case target do
          {:shop_item, target} ->
            target = Map.put(target, :level, target.level || character.level)
            look_at_item(character, target, shop_item: true)

          target ->
            target = Map.put(target, :level, target.level || character.level)
            look_at_item(character, target)
        end

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
            "#{Mobile.colored_name(character)} peeks in from #{Room.enter_direction(mirror_exit["direction"])}!"

          Mobile.send_scroll(observer, "<p><span class='dark-magenta'>#{message}</span></p>")

        _ ->
          :noop
      end)
    end
  end

  def look_at_mobile(%Character{monster: nil} = character, %Character{} = observer, _room) do
    Mobile.send_scroll(
      observer,
      "<p>The incorporeal ghost of #{character.name} hovers before you.</p>"
    )
  end

  def look_at_mobile(%Character{} = target, %{} = character, _room) do
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
      target.monster.description
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
          "<p><span class='dark-cyan'>#{worn_on}</span><span class='dark-green'>#{Item.colored_name(item, character: target)}</span></p>"
        )
      end)

      Mobile.send_scroll(character, "<br>")
    end
  end

  def look_at_mobile(%{} = target, %{} = character, room) do
    hp_description = Mobile.hp_description(target)

    hp_description =
      "{{target:He appears/She appears/They appear}} to be #{hp_description}."
      |> interpolate(%{"target" => target})

    description =
      target.description
      |> interpolate(%{"target" => target})

    Mobile.send_scroll(character, "<p>#{description}</p>")
    Mobile.send_scroll(character, "<p>#{hp_description}\n\n</p>")

    if character.admin do
      accuracy = Mobile.accuracy_at_level(target, target.level, room)
      dodge = Mobile.dodge_at_level(target, target.level, room)
      perception = Mobile.dodge_at_level(target, target.level, room)

      Mobile.send_scroll(
        character,
        "<p class='white'>Level #{target.level}</p>"
      )

      Mobile.send_scroll(
        character,
        "<p class='dark-magenta'>---------------------------------------------------------------------------</p>"
      )

      Mobile.send_scroll(
        character,
        "<p>accuracy:   #{accuracy}%                             dodge: #{dodge}%</p>"
      )

      Mobile.send_scroll(character, "<p>perception: #{perception}%</p>")
      Mobile.send_scroll(character, "<br>")
    end
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
    trait_descs =
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

    skill_descs =
      item.affix_skills
      |> Enum.reject(&is_nil(&1.description))
      |> Enum.map(fn instance_affix_skill ->
        values =
          instance_affix_skill.affix_skill.value
          |> Map.put("skill", instance_affix_skill.affix_skill.skill.name)

        ApathyDrive.Text.interpolate(instance_affix_skill.description, values)
      end)

    (trait_descs ++ skill_descs)
    |> Enum.join("\n")
  end

  def socket(item, type) do
    description =
      item.socketable_item_affixes
      |> Enum.filter(&(&1.item_type.name == type))
      |> List.flatten()
      |> Enum.map(& &1.affix.affixes_traits)
      |> List.flatten()
      |> Enum.reject(&is_nil(&1.description))
      |> Enum.map(fn affix_trait ->
        value =
          if is_integer(affix_trait.value) do
            %{"amount" => affix_trait.value}
          else
            affix_trait.value
          end

        ApathyDrive.Text.interpolate(affix_trait.description, value)
      end)
      |> Enum.join("\n")

    "\n<span style='color: #4850B8'>#{description}</span>"
  end

  def sockets(%Item{type: "Stone"} = item) do
    tooltip = "\nCan Be Inserted into Socketed Items\n"

    tooltip = tooltip <> "\nWeapons#{socket(item, "Weapon")}"
    tooltip = tooltip <> "\nHelm or Torso#{socket(item, "Helm")}"
    tooltip <> "\nShield#{socket(item, "Any Shield")}"
  end

  def sockets(%Item{} = item) do
    item.sockets
    |> Enum.sort_by(& &1.number)
    |> Enum.map(fn
      %Socket{socketed_item: nil} ->
        "\n<span class='dark-grey'>empty socket</span>"

      %Socket{socketed_item: %Item{} = socketed_item} ->
        description =
          socketed_item.socketable_item_affixes
          |> Enum.filter(&(&1.item_type in item.item_types))
          |> List.flatten()
          |> Enum.map(& &1.affix.affixes_traits)
          |> List.flatten()
          |> Enum.reject(&is_nil(&1.description))
          |> Enum.map(fn affix_trait ->
            value =
              if is_integer(affix_trait.value) do
                %{"amount" => affix_trait.value}
              else
                affix_trait.value
              end

            ApathyDrive.Text.interpolate(affix_trait.description, value)
          end)
          |> Enum.join("\n")

        "\n#{Item.colored_name(socketed_item)}\n<span style='color: #4850B8'>#{description}</span>"
    end)
    |> Enum.join("")
  end

  def affix_description(character, "DefensePerLevel", description, val) do
    ApathyDrive.Text.interpolate(description, %{"amount" => val * character.level})
  end

  def affix_description(_character, _trait, description, _val), do: description

  def defense(item, character, opts) do
    value =
      Systems.Effect.effect_bonus(item, "Defense") +
        Systems.Effect.effect_bonus(item, "DefensePerLevel") * character.level

    %{min: min, max: max} = Item.ac_for_item(item)

    cond do
      opts[:shop_item] ->
        if min + value <= 0 and max + value <= 0 do
          ""
        else
          "\nDefense: #{min + value}-#{max + value}"
        end

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
    modifier = (100 + (Systems.Effect.effect_bonus(item, "Damage%") || 0)) / 100

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

  def required_strength(character, %Item{} = item) do
    if (strength = Item.required_strength(item)) > 0 do
      if Mobile.attribute_value(character, :strength) >= strength do
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
      if Mobile.attribute_value(character, :agility) >= agility do
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

  def weapon_speed(_character, %Item{speed: nil}), do: ""

  def weapon_speed(_character, %Item{speed: _speed, type_id: nil} = _item), do: ""

  def weapon_speed(%Character{} = character, %Item{} = weapon) do
    type = Item.titleize(weapon.weapon_type)

    level = character.level
    encumbrance = Character.encumbrance(character)
    max_encumbrance = Character.max_encumbrance(character)
    agility = Mobile.attribute_value(character, :agility)

    combat_level = Character.combat_level(character)

    cost =
      weapon.speed * 1000 /
        ((level * (combat_level + 2) + 45) * (agility + 150) *
           1500 /
           9000.0)

    speed =
      trunc(
        cost * (Float.floor(Float.floor(encumbrance / max_encumbrance * 100) / 2.0) + 75) / 100.0
      )

    desc = weapon_speeds(speed)

    ias = Mobile.ability_value(character, "IncreasedAttackSpeed") || 0

    ias_multiplier = (100 - ias) / 100

    ias_energy = trunc(speed * ias_multiplier)

    enhanced_desc = weapon_speeds(ias_energy)

    if enhanced_desc != desc do
      "\n#{type}: <span style='color: #908858'>#{enhanced_desc} Attack Speed</span>"
    else
      "\n#{type}: #{enhanced_desc} Attack Speed"
    end
  end

  def weapon_speeds(energy) when energy > 500, do: "Very Slow"
  def weapon_speeds(energy) when energy > 333 and energy <= 800, do: "Slow"
  def weapon_speeds(energy) when energy > 250 and energy <= 500, do: "Normal"
  def weapon_speeds(energy) when energy > 200 and energy <= 330, do: "Fast"
  def weapon_speeds(energy) when energy <= 200, do: "Very Fast"

  def item_tooltip(character, item, opts \\ [])

  def item_tooltip(%Character{} = character, %Item{} = item, opts) do
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
    #{Item.colored_name(item, titleize: true, no_tooltip: true)}#{description(item)}#{damage(item, character)}#{defense(item, character, opts)}#{required_level(character, item)}#{required_strength(character, item)}#{required_agility(character, item)}#{weapon_speed(character, item)}#{sockets(item)}
    <span style='color: #4850B8'>#{affix_trait_descriptions(item, character)}</span>

    Sells For: #{value}
    """
  end

  def item_tooltip(%Character{} = _character, _item, _opts), do: ""

  def description(%Item{description: nil}), do: ""

  def description(%Item{type: "Key", description: description} = item) do
    require Ecto.Query

    room_exit =
      ApathyDrive.RoomExit
      |> Ecto.Query.where(item_id: ^item.id)
      |> Ecto.Query.preload([:exit, :room, :destination])
      |> Ecto.Query.first()
      |> Repo.one()

    if room_exit do
      door_type =
        case room_exit.exit.kind do
          "Gate" ->
            "gate"

          _ ->
            "door"
        end

      "\n#{description} It unlocks the #{door_type} between #{room_exit.room.name} and #{room_exit.destination.name}.\n"
    else
      "\n#{description}\n"
    end
  end

  def description(%Item{description: description}), do: "\n#{description}\n"

  def look_at_item(character, item, opts \\ [])

  def look_at_item(%Character{} = character, %Item{type: "Sign"} = item, _opts) do
    Mobile.send_scroll(
      character,
      "<p>#{item.description}</p>"
    )
  end

  def look_at_item(%Character{} = character, %Item{getable: false, hidden: true} = item, _opts) do
    Mobile.send_scroll(
      character,
      "<p>#{item.description}</p>"
    )
  end

  def look_at_item(%Character{} = character, %Item{} = item, opts) do
    item = item_tooltip(character, item, opts)
    Mobile.send_scroll(character, "<p class='item'>#{item}</span>")
  end

  def look_at_item(mobile, %{description: description}, _opts) do
    Mobile.send_scroll(mobile, "<p>#{description}</p>")
  end

  def look_at_item(%Character{} = mobile, item_name, _opts) do
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
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{on_hit || "100"}% chance of:</span> <span class='dark-cyan'>#{names}</span></p>"
    )
  end

  def display_trait(character, _item, {"Grant", abilities}, indent) do
    names =
      abilities
      |> Enum.map(& &1.name)
      |> ApathyDrive.Commands.Inventory.to_sentence()

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Grants the ability to cast:</span> <span class='dark-cyan'>#{names}</span></p>"
    )
  end

  def display_trait(character, _item, {"Damage", list}, indent) when is_list(list) do
    Enum.each(list, fn %{kind: kind, damage_type: type, min: min, max: max} ->
      Mobile.send_scroll(
        character,
        "<p>#{String.pad_trailing("", indent)}<span class='dark-cyan'>#{min}-#{max} #{kind} #{String.downcase(type)} damage</span></p>"
      )
    end)
  end

  def display_trait(character, item, {"Powerstone", _}, indent) do
    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Powerstone:</span> <span class='dark-cyan'>#{trunc(item.uses)}/#{item.max_uses} mana</span></p>"
    )
  end

  def display_trait(character, _item, {"RemoveSpells", list}, indent) do
    list =
      list
      |> Enum.map(&Ability.find/1)
      |> Enum.map(& &1.name)

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Remove Spells:</span> <span class='dark-cyan'>#{inspect(list)}</span></p>"
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
        "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>Claimed by:</span> <span class='dark-cyan'>#{owner.name}</span></p>"
      )
    end
  end

  def display_trait(_character, _item, {"timers", _list}, _indent), do: :noop
  def display_trait(_character, _item, {"effect_ref", _list}, _indent), do: :noop

  def display_trait(character, _item, {trait, list}, indent) when is_list(list) do
    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{trait}:</span> <span class='dark-cyan'>#{inspect(list)}</span></p>"
    )
  end

  def display_trait(_character, _item, {_trait, %{}}, _indent), do: :noop

  def display_trait(character, _item, {trait, value}, indent) do
    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{trait}:</span> <span class='dark-cyan'>#{inspect(value)}</span></p>"
    )
  end

  def display_enchantment(_character, %Item{type: "Stone"}), do: :noop

  def display_enchantment(character, %Item{enchantments: list}) when length(list) > 0 do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Enchantments:</span> <span class='dark-cyan'>#{ApathyDrive.Commands.Inventory.to_sentence(list)}</span></p>"
    )
  end

  def display_enchantment(_character, %Item{}), do: :noop

  defp find_item(%Character{inventory: items, equipment: equipment} = character, item) do
    gems =
      character
      |> Ecto.assoc(:characters_items)
      |> Ecto.Query.preload(
        item: [
          socketable_item_affixes: [
            :item_type,
            affix: [
              affixes_traits: [:trait, :affix]
            ]
          ]
        ]
      )
      |> Repo.all()
      |> Enum.map(fn ci ->
        ci.item
        |> Item.with_traits()
        |> Item.load_required_races_and_classes()
        |> Item.load_item_abilities()
      end)

    item =
      (items ++ equipment ++ gems)
      |> Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        nil

      %{} = item ->
        item
    end
  end
end
