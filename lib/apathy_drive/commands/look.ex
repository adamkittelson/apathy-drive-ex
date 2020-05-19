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
    Match,
    Mobile,
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
            if ApathyDrive.Commands.Wear.worn_on_max(item) > 1 do
              worn_on =
                item.limb
                |> String.split(" ")
                |> Enum.map(&String.capitalize/1)
                |> Enum.join(" ")

              String.pad_trailing("(#{worn_on})", 15)
            else
              String.pad_trailing("(#{item.worn_on})", 15)
            end
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

    attack_interval = Regeneration.duration_for_energy(character, ability.energy)

    {min_damage, max_damage} =
      Enum.reduce(ability.traits["Damage"], {0, 0}, fn damage, {min_damage, max_damage} ->
        {min_damage + damage.min, max_damage + damage.max}
      end)

    average = (min_damage + max_damage) / 2

    dps = Float.round(average / (attack_interval / 1000), 2)

    %{dps: dps, min_damage: min_damage, max_damage: max_damage, ability: ability}
  end

  def look_at_item(%Character{} = character, %Item{type: "Weapon"} = item) do
    ability = Character.ability_for_weapon(character, item, false)
    damage = weapon_damage(character, ability)

    value =
      %Shop{cost_multiplier: 1}
      |> Shop.sell_price(character, item)
      |> Currency.set_value()
      |> Currency.to_string()
      |> case do
        "" -> "FREE"
        value -> value
      end

    Mobile.send_scroll(character, "<p>#{item.description}</p>")

    item.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "StatusMessage"))
    |> Enum.map(& &1["StatusMessage"])
    |> Enum.each(fn effect_message ->
      Mobile.send_scroll(character, "<p>#{effect_message}</p>")
    end)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Kind:</span> <span class='dark-cyan'>#{item.weapon_type}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>DPS:</span> <span class='dark-cyan'>#{damage.dps} (#{
        trunc(damage.min_damage)
      }-#{trunc(damage.max_damage)} @ #{trunc(damage.ability.energy / 10)}% energy per swing)</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Weight:</span> <span class='dark-cyan'>#{item.weight}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Value:</span> <span class='dark-cyan'>#{value}</span></p>"
    )

    display_traits(character, item)

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
    skill = Item.skill_for_character(character, item)

    modifier =
      if skill == 0 do
        0.1
      else
        skill / character.level
      end

    traits =
      item.effects
      |> Map.values()
      |> Enum.reduce(%{}, &Trait.merge_traits(&2, &1))
      |> Ability.process_duration_traits(character, character, nil)
      |> Map.put_new("AC", 0)
      |> update_in(["AC"], &trunc(&1 * modifier))
      |> Map.put_new("MR", 0)
      |> update_in(["MR"], &trunc(&1 * modifier))

    Enum.each(traits, &display_trait(character, item, &1, indent))
  end

  def display_trait(_character, _item, {"AC", 0}, _indent), do: :noop
  def display_trait(_character, _item, {"MR", 0}, _indent), do: :noop
  def display_trait(_character, _item, {"RemoveMessage", _msg}, _indent), do: :noop
  def display_trait(_character, _item, {"stack_count", _msg}, _indent), do: :noop
  def display_trait(_character, _item, {"stack_key", _msg}, _indent), do: :noop

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

  def display_trait(character, item, {"OnHit", abilities}, indent) do
    on_hit = Systems.Effect.effect_bonus(item, "OnHit%")

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>#{on_hit || "100"}% chance of:</span></p>"
    )

    Enum.each(abilities, fn ability ->
      display_traits(character, Map.put(item, :effects, %{1 => ability.traits}), 2)
    end)
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

  def display_trait(character, _item, {"AC%", value}, indent) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(value)

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>AC:</span> <span class='dark-cyan'>#{
        ac_from_percent
      }</span></p>"
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

  def display_trait(character, _item, {"ClassOk", list}, indent) do
    list =
      list
      |> List.wrap()
      |> Enum.map(&ApathyDrive.Repo.get!(ApathyDrive.Class, &1))
      |> Enum.map(& &1.name)
      |> ApathyDrive.Commands.Inventory.to_sentence()

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_trailing("", indent)}<span class='dark-green'>ClassOk:</span> <span class='dark-cyan'>#{
        list
      }</span></p>"
    )
  end

  def display_trait(_character, _item, {"timers", _list}, _indent), do: :noop

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
        value
      }</span></p>"
    )
  end

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
