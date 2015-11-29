defmodule ApathyDrive.Mobile do
  alias ApathyDrive.Mobile
  alias ApathyDrive.Repo
  alias ApathyDrive.PubSub
  use GenServer
  import Systems.Text
  import BlockTimer

  defstruct spirit: nil,
            socket: nil,
            hp: nil,
            max_hp: nil,
            strength: nil,
            strength_per_level: 0,
            agility: nil,
            agility_per_level: 0,
            will: nil,
            will_per_level: 0,
            description: "Some temporary description.",
            mana: nil,
            max_mana: nil,
            effects: %{},
            pid: nil,
            room_id: nil,
            alignment: nil,
            name: nil,
            keywords: [],
            enter_message: "{{name}} enters from {{direction}}.",
            exit_message: "{{name}} leaves {{direction}}.",
            death_message: "{{name}} dies.",
            gender: nil,
            greeting: nil,
            abilities: [],
            level: nil,
            hate: %{},
            timers: %{},
            flags: [],
            experience: nil,
            monster_template_id: nil,
            physical_defense: 0,
            magical_defense: 0,
            fire_resistance: 0,
            ice_resistance: 0,
            stone_resistance: 0,
            lightning_resistance: 0,
            water_resistance: 0,
            poison_resistance: 0,
            attack_target: nil,
            auto_attack_interval: 2.0,
            highest_armour_grade: 0

  def start_link(state \\ %{}, opts \\ []) do
    GenServer.start_link(__MODULE__, Map.merge(%Mobile{}, state), opts)
  end

  def use_ability(pid, command, arguments) do
    GenServer.cast(pid, {:use_ability, command, arguments})
  end

  def add_experience(mobile, exp) do
    GenServer.cast(mobile, {:add_experience, exp})
  end

  def add_form(mobile, item) do
    GenServer.cast(mobile, {:add_form, item})
  end

  def data_for_who_list(pid) do
    GenServer.call(pid, :data_for_who_list)
  end

  def ability_list(pid) do
    GenServer.call(pid, :ability_list)
  end

  def greeting(pid) do
    GenServer.call(pid, :greeting)
  end

  def look_name(pid) do
    GenServer.call(pid, :look_name)
  end

  def spirit_id(pid) do
    GenServer.call(pid, :spirit_id)
  end

  def get_look_data(pid) do
    GenServer.call(pid, :look_data)
  end

  def match_data(pid) do
    GenServer.call(pid, :match_data)
  end

  def room_id(pid) do
    GenServer.call(pid, :room_id)
  end

  def name(pid) do
    GenServer.call(pid, :name)
  end

  def enter_message(pid) do
    GenServer.call(pid, :enter_message)
  end

  def exit_message(pid) do
    GenServer.call(pid, :exit_message)
  end

  def score_data(pid) when is_pid(pid) do
    GenServer.call(pid, :score_data)
  end
  def score_data(mobile) do
    effects =
      mobile.effects
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "effect_message")))
      |> Enum.map(&(&1["effect_message"]))

    %{name: mobile.spirit.name,
      class: (mobile.monster_template_id && mobile.name) || mobile.spirit.class.name,
      level: mobile.spirit.level,
      experience: mobile.spirit.experience,
      hp: mobile.hp,
      max_hp: mobile.max_hp,
      mana: mobile.mana,
      max_mana: mobile.max_mana,
      strength: strength(mobile),
      agility: agility(mobile),
      will: will(mobile),
      effects: effects,
      physical_defense: physical_defense(mobile),
      magical_defense: magical_defense(mobile),
      physical_damage: physical_damage(mobile),
      magical_damage: magical_damage(mobile)}
  end


  def value(pid) do
    GenServer.call(pid, :value)
  end

  def display_experience(pid) do
    GenServer.cast(pid, :display_experience)
  end

  def class_chat(pid, message) do
    GenServer.cast(pid, {:class_chat, message})
  end

  def aligned_spirit_name(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :aligned_spirit_name)
  end
  def aligned_spirit_name(%Mobile{spirit: %Spirit{name: name, class: %{alignment: "good"}}}) do
    "<span class='white'>#{name}</span>"
  end
  def aligned_spirit_name(%Mobile{spirit: %Spirit{name: name, class: %{alignment: "neutral"}}}) do
    "<span class='dark-cyan'>#{name}</span>"
  end
  def aligned_spirit_name(%Mobile{spirit: %Spirit{name: name, class: %{alignment: "evil"}}}) do
    "<span class='magenta'>#{name}</span>"
  end

  def look_at_item(%Mobile{} = mobile, item) do
    Mobile.send_scroll(mobile, "\n\n")

    Mobile.send_scroll(mobile, "<p><span class='cyan'>#{item["name"]}</span></p>")
    Mobile.send_scroll(mobile, "<p>#{item["description"]}</p>\n\n")

    current =
      mobile
      |> Mobile.score_data

    {:reply, {:ok, %{equipped: _}}, equipped} =
      mobile
      |> Mobile.equip_item(item)

    equipped = Mobile.score_data(equipped)

    score_data =
      current
      |> Map.take([:max_hp, :max_mana, :physical_damage, :magical_damage, :physical_defense, :magical_defense, :strength, :agility, :will])
      |> Enum.reduce(%{}, fn({key, val}, values) ->
           Map.put(values, key, value(val, equipped[key]))
         end)

    Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>Changes if Equipped:</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Max HP:</span> <span class='dark-cyan'>#{score_data.max_hp}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Max Mana:</span> <span class='dark-cyan'>#{score_data.max_mana}</span></p>")

    Mobile.send_scroll(mobile, "\n\n")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Physical Damage:</span> <span class='dark-cyan'>#{score_data.physical_damage}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Magical Damage:</span>  <span class='dark-cyan'>#{score_data.magical_damage}</span></p>")
    Mobile.send_scroll(mobile, "\n\n")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Physical Defense:</span> <span class='dark-cyan'>#{score_data.physical_defense}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Magical Defense:</span> <span class='dark-cyan'>#{score_data.magical_defense}</span></p>")
    Mobile.send_scroll(mobile, "\n\n")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Strength:</span> <span class='dark-cyan'>#{score_data.strength}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Agility:</span>  <span class='dark-cyan'>#{score_data.agility}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Will:</span>     <span class='dark-cyan'>#{score_data.will}</span></p>")
  end
  def look_at_item(mobile, item) do
    GenServer.call(mobile, {:look_at_item, item})
  end

  def interpolation_data(%Mobile{} = mobile),  do: %{name: mobile.name, gender: mobile.gender}
  def interpolation_data(pid) when is_pid(pid) do
    GenServer.call(pid, :interpolation_data)
  end

  def display_prompt(%Mobile{socket: socket} = mobile) do
    ApathyDrive.PubSub.broadcast_from! socket, "spirits:#{mobile.spirit.id}:socket", :go_home

    send(socket, {:disable_element, "#prompt"})
    send(socket, {:disable_element, "#command"})
    send(socket, {:scroll, "<p><span id='prompt'>#{prompt(mobile)}</span><input id='command' size='50' class='prompt'></input></p>"})
    send(socket, {:focus_element, "#command"})
    send(socket, :up)
  end

  def update_prompt(%Mobile{socket: nil}), do: :noop

  def update_prompt(%Mobile{socket: socket} = mobile) do
    send(socket, {:update_prompt, prompt(mobile)})
  end

  def prompt(%Mobile{} = mobile) do
    "[HP=#{trunc(mobile.hp)}/MA=#{trunc(mobile.mana)}]:"
  end

  def alignment_color(%{alignment: "evil"}),    do: "magenta"
  def alignment_color(%{alignment: "good"}),    do: "white"
  def alignment_color(%{alignment: "neutral"}), do: "dark-cyan"

  def blind?(mobile) do
    GenServer.call(mobile, :blind?)
  end

  def display_inventory(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :display_inventory)
  end
  def display_inventory(%Mobile{spirit: nil}), do: nil
  def display_inventory(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile) do
    if equipment |> Enum.any? do
      Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")

      equipment
      |> Enum.each fn(item) ->
        send_scroll(mobile, "<p><span class='dark-green'>#{String.ljust(item["name"], 23)}</span><span class='dark-cyan'>(#{item["worn_on"]})</span></p>")
      end
      send_scroll(mobile, "<br>")
    end

    items = inventory |> Enum.map(&(&1["name"]))
    if items |> Enum.count > 0 do
      send_scroll(mobile, "<p>You are carrying #{Enum.join(items, ", ")}</p>")
    else
      send_scroll(mobile, "<p>You are carrying nothing.</p>")
    end

    display_encumbrance(mobile)
  end

  def get_item(mobile, item) do
    GenServer.call(mobile, {:get_item, item})
  end

  def drop_item(mobile, item) do
    GenServer.call(mobile, {:drop_item, item})
  end

  def destroy_item(mobile, item) do
    GenServer.call(mobile, {:destroy_item, item})
  end

  def equip_item(mobile, item) when is_pid(mobile) do
    GenServer.call(mobile, {:equip_item, item})
  end

  def equip_item(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, %{"worn_on" => worn_on} = item) do
    cond do
      Enum.count(equipment, &(&1["worn_on"] == worn_on)) >= worn_on_max(item) ->
        item_to_remove =
          equipment
          |> Enum.find(&(&1["worn_on"] == worn_on))

        equipment =
          equipment
          |> List.delete(item_to_remove)
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.insert_at(-1, item_to_remove)
          |> List.delete(item)

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> set_highest_armour_grade
                   |> set_abilities
                   |> set_max_mana
                   |> set_mana
                   |> set_max_hp
                   |> set_hp
                   |> set_physical_defense
                   |> set_magical_defense

        {:reply, {:ok, %{equipped: item, unequipped: [item_to_remove]}}, mobile}
      conflicting_worn_on(worn_on) |> Enum.any? ->
        items_to_remove =
          equipment
          |> Enum.filter(&(&1["worn_on"] in conflicting_worn_on(worn_on)))

        equipment =
          equipment
          |> Enum.reject(&(&1 in items_to_remove))
          |> List.insert_at(-1, item)

        inventory =
          items_to_remove
          |> Enum.reduce(inventory, fn(item_to_remove, inv) ->
               List.insert_at(inv, -1, item_to_remove)
             end)
          |> List.delete(item)

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> set_highest_armour_grade
                   |> set_abilities
                   |> set_max_mana
                   |> set_mana
                   |> set_max_hp
                   |> set_hp
                   |> set_physical_defense
                   |> set_magical_defense

        {:reply, {:ok, %{equipped: item, unequipped: items_to_remove}}, mobile}
      true ->
        equipment =
          equipment
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.delete(item)

        mobile = put_in(mobile.spirit.inventory, inventory)
        mobile = put_in(mobile.spirit.equipment, equipment)
                 |> set_highest_armour_grade
                 |> set_abilities
                 |> set_max_mana
                 |> set_mana
                 |> set_max_hp
                 |> set_hp
                 |> set_physical_defense
                 |> set_magical_defense

        {:reply, {:ok, %{equipped: item}}, mobile}
    end
  end

  def unequip_item(mobile, item) do
    GenServer.call(mobile, {:unequip_item, item})
  end

  def find_item(mobile, item) do
    GenServer.call(mobile, {:find_item, item})
  end

  def display_encumbrance(%Mobile{spirit: nil}), do: nil
  def display_encumbrance(%Mobile{} = mobile) do
    current = current_encumbrance(mobile)
    max = max_encumbrance(mobile)
    percent = trunc((current / max) * 100)

    display_encumbrance(mobile, current, max, percent)
  end
  def display_encumbrance(%Mobile{} = mobile, current, max, percent) when percent < 17 do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> None [#{percent}%]</p>")
  end
  def display_encumbrance(%Mobile{} = mobile, current, max, percent) when percent < 34 do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> <span class='dark-green'>Light [#{percent}%]</span></p>")
  end
  def display_encumbrance(%Mobile{} = mobile, current, max, percent) when percent < 67 do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> <span class='dark-yellow'>Medium [#{percent}%]</span></p>")
  end
  def display_encumbrance(%Mobile{} = mobile, current, max, percent) do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> <span class='dark-red'>Heavy [#{percent}%]</span></p>")
  end

  def max_encumbrance(%Mobile{} = mobile) do
    strength(mobile) * 48
  end

  def current_encumbrance(%Mobile{spirit: %Spirit{inventory: inventory}}) do
    inventory
    |> Enum.reduce(0, fn(item, encumbrance) ->
        encumbrance + item["weight"]
       end)
  end

  def remaining_encumbrance(%Mobile{} = mobile) do
    max_encumbrance(mobile) - current_encumbrance(mobile)
  end

  def held(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :held?)
  end
  def held(%Mobile{effects: effects} = mobile) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "held")
       end)
    |> held(mobile)
  end
  def held(nil, %Mobile{}), do: false
  def held(%{"effect_message" => message}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>#{message}</p>")
    true
  end

  def silenced(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :silenced?)
  end
  def silenced(%Mobile{effects: effects} = mobile, %{"mana_cost" => cost}) when cost > 0 do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "silenced")
       end)
    |> silenced(mobile)
  end
  def silenced(%Mobile{}, %{}), do: false
  def silenced(nil, %Mobile{}), do: false
  def silenced(%{"effect_message" => message}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>#{message}</p>")
  end

  def confused(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :confused?)
  end
  def confused(%Mobile{effects: effects} = mobile) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "confused") && (effect["confused"] >= :random.uniform(100))
       end)
    |> held(mobile)
  end
  def confused(nil, %Mobile{}), do: false
  def confused(%{"confusion_message" => %{"user" => user_message, "spectator" => spectator_message}}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>#{user_message}</p>")
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{mobile.room_id}", "scroll", %{:html => "<p>#{interpolate(spectator_message, %{"user" => mobile})}</p>"}
    true
  end
  def confused(%{}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>You fumble in confusion!</p>")
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{mobile.room_id}", "scroll", %{:html => "<p>#{interpolate("{{user}} fumbles in confusion!", %{"user" => mobile})}</p>"}
    true
  end

  def reduce_damage(%Mobile{} = mobile, "physical defense") do
    1 - (0.00044 * physical_defense(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "magical defense") do
    1 - (0.00044 * magical_defense(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "fire resistance") do
    1 - (0.01 * fire_resistance(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "ice resistance") do
    1 - (0.01 * ice_resistance(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "stone resistance") do
    1 - (0.01 * stone_resistance(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "lightning resistance") do
    1 - (0.01 * lightning_resistance(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "water resistance") do
    1 - (0.01 * water_resistance(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "poison resistance") do
    1 - (0.01 * poison_resistance(mobile))
  end

  def reduce_damage(%Mobile{}, damage, nil), do: damage
  def reduce_damage(%Mobile{}, damage, []),  do: damage
  def reduce_damage(%Mobile{} = mobile, damage, mitigated_by) when is_list(mitigated_by) do
    multiplier = Enum.reduce(mitigated_by, 1, fn(mitigating_factor, multiplier) ->
      multiplier * reduce_damage(mobile, mitigating_factor)
    end)

    max(0, trunc(damage * multiplier))
  end

  def physical_defense(%Mobile{} = mobile) do
    mobile.physical_defense
  end

  def magical_defense(%Mobile{} = mobile) do
    mobile.magical_defense
  end

  def fire_resistance(%Mobile{} = mobile) do
    mobile.fire_resistance
  end

  def ice_resistance(%Mobile{} = mobile) do
    mobile.ice_resistance
  end

  def stone_resistance(%Mobile{} = mobile) do
    mobile.stone_resistance
  end

  def lightning_resistance(%Mobile{} = mobile) do
    mobile.lightning_resistance
  end

  def water_resistance(%Mobile{} = mobile) do
    mobile.water_resistance
  end

  def poison_resistance(%Mobile{} = mobile) do
    mobile.poison_resistance
  end

  def effect_bonus(%Mobile{effects: effects}, name) do
    effects
    |> Map.values
    |> Enum.map(fn
         (%{} = effect) ->
           Map.get(effect, name, 0)
         (_) ->
           0
       end)
    |> Enum.sum
  end

  def send_scroll(mobile, message) when is_pid(mobile) do
    send mobile, {:send_scroll, message}
  end
  def send_scroll(%Mobile{socket: nil} = mobile, _html),  do: mobile
  def send_scroll(%Mobile{socket: socket} = mobile, html) do
    send(socket, {:scroll, html})
    mobile
  end

  def init(%Mobile{spirit: nil} = mobile) do
    :random.seed(:os.timestamp)

    mobile =
      mobile
      |> Map.put(:pid, self)
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> set_physical_defense
      |> set_magical_defense
      |> TimerManager.call_every({:monster_regen,    1_000, fn -> send(self, :regen) end})
      |> TimerManager.call_every({:periodic_effects, 3_000, fn -> send(self, :apply_periodic_effects) end})
      |> TimerManager.call_every({:monster_ai,       5_000, fn -> send(self, :think) end})

      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")
      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")
      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:spawned_monsters")

    {:ok, mobile}
  end
  def init(%Mobile{spirit: spirit_id, socket: socket} = mobile) do
    :random.seed(:os.timestamp)

    spirit = Repo.get(Spirit, spirit_id)
             |> Repo.preload(:class)

    ApathyDrive.PubSub.subscribe(self, "spirits:online")
    ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
    ApathyDrive.PubSub.subscribe(self, "chat:gossip")
    ApathyDrive.PubSub.subscribe(self, "chat:#{String.downcase(spirit.class.name)}")
    ApathyDrive.PubSub.subscribe(socket, "spirits:#{spirit.id}:socket")

    mobile =
      mobile
      |> Map.put(:spirit, spirit)
      |> Map.put(:pid, self)
      |> Map.put(:room_id, spirit.room_id)
      |> Map.put(:alignment, spirit.class.alignment)
      |> Map.put(:strength, spirit.class.strength)
      |> Map.put(:strength_per_level, spirit.class.strength_per_level)
      |> Map.put(:agility, spirit.class.agility)
      |> Map.put(:agility_per_level, spirit.class.agility_per_level)
      |> Map.put(:will, spirit.class.will)
      |> Map.put(:will_per_level, spirit.class.will_per_level)
      |> Map.put(:name, spirit.name)
      |> Map.put(:level, spirit.level)
      |> set_highest_armour_grade
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> set_physical_defense
      |> set_magical_defense
      |> TimerManager.call_every({:monster_regen,    1_000, fn -> send(self, :regen) end})
      |> TimerManager.call_every({:periodic_effects, 3_000, fn -> send(self, :apply_periodic_effects) end})

    ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")
    ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")

    update_prompt(mobile)

    {:ok, mobile}
  end

  def able_to_possess?(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :able_to_possess?)
  end

  def attackable?(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :attackable?)
  end

  def attack(mobile, target) do
    GenServer.call(mobile, {:attack, target})
  end

  def possess(mobile, spirit_id, socket) when is_pid(mobile) do
    GenServer.call(mobile, {:possess, spirit_id, socket})
  end

  def unpossess(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :unpossess)
  end

  def set_highest_armour_grade(%Mobile{spirit: nil} = mobile) do
    Map.put(mobile, :highest_armour_grade, 0)
  end
  def set_highest_armour_grade(%Mobile{spirit: %Spirit{inventory: [], equipment: []}} = mobile) do
    Map.put(mobile, :highest_armour_grade, 0)
  end
  # if a spirit is holding anything ot loses its ethereal status
  def set_highest_armour_grade(%Mobile{spirit: %Spirit{inventory: _inv, equipment: []}} = mobile) do
    Map.put(mobile, :highest_armour_grade, 1)
  end
  def set_highest_armour_grade(%Mobile{spirit: %Spirit{equipment: equipment}} = mobile) do
    highest_grade =
      equipment
      |> Enum.max_by(&(&1["grade"]))
      |> Map.get("grade")

    Map.put(mobile, :highest_armour_grade, highest_grade)
  end

  def set_abilities(%Mobile{spirit: nil} = mobile), do: adjust_mana_costs(mobile)
  def set_abilities(%Mobile{spirit: spirit, level: level} = mobile) do
    abilities =
     spirit.class.abilities
     |> Enum.filter(&(Map.get(&1, "level", 0) <= level))
     |> Enum.reject(&(Map.get(&1, "min_armour_type", 1) > mobile.highest_armour_grade))
     |> Enum.reject(&(Map.get(&1, "max_armour_type", 10) < mobile.highest_armour_grade))
     |> add_abilities_from_equipment(spirit.equipment)

    mobile
    |> Map.put(:abilities, abilities)
    |> set_passive_effects
    |> adjust_mana_costs
  end

  def add_abilities_from_equipment(abilities, equipment) do
    abilities ++ Enum.flat_map(equipment, &(&1["abilities"]))
  end

  def set_passive_effects(%Mobile{abilities: []} = mobile) do
    remove_passive_effects(mobile, passive_effects(mobile))
  end
  def set_passive_effects(%Mobile{abilities: abilities} = mobile) do
    original_passives = passive_effects(mobile)

    new_passives =
      abilities
      |> Enum.filter(&(Map.has_key?(&1, "passive_effects")))
      |> Enum.map(&(%{"name" => "passive_#{&1["name"]}", "passive_effects" => &1["passive_effects"]}))

    passives_to_remove = original_passives -- Enum.map(new_passives, &(Map.get(&1, "name")))

    mobile = remove_passive_effects(mobile, passives_to_remove)

    new_passives
    |> Enum.reduce(mobile, fn(%{"name" => name, "passive_effects" => effect}, new_mobile) ->
         Systems.Effect.add_effect(new_mobile, name, effect)
       end)
  end

  def remove_passive_effects(%Mobile{} = mobile, effect_keys_to_remove) do
    Enum.reduce(effect_keys_to_remove, mobile, fn(effect_key, new_mobile) ->
      Systems.Effect.remove(new_mobile, effect_key)
    end)
  end

  def passive_effects(%Mobile{effects: effects}) do
    effects
    |> Map.keys
    |> Enum.filter(&(String.starts_with?(&1, "passive")))
  end

  def adjust_mana_costs(%Mobile{} = mobile) do
    abilities =
      mobile.abilities
      |> Enum.map(&(adjust_mana_cost(mobile, &1)))

    Map.put(mobile, "abilities", abilities)
  end
  def adjust_mana_cost(%Mobile{level: level}, %{"mana_cost" => base} = ability) do
    Map.put(ability, "mana_cost",  trunc(base + base * ((level * 0.1) * ((level * 0.1)))))
  end
  def adjust_mana_cost(%Mobile{}, %{} = ability), do: ability

  def set_mana(%Mobile{mana: nil, max_mana: max_mana} = mobile) do
    Map.put(mobile, :mana, max_mana)
  end
  def set_mana(%Mobile{mana: mana, max_mana: max_mana} = mobile) do
    Map.put(mobile, :mana, min(mana, max_mana))
  end

  def set_max_mana(%Mobile{} = mobile) do
    Map.put(mobile, :max_mana, trunc(will(mobile) * (0.9 + (0.09 * mobile.level))))
  end

  def set_hp(%Mobile{hp: nil, max_hp: max_hp} = mobile) do
    Map.put(mobile, :hp, max_hp)
  end
  def set_hp(%Mobile{hp: hp, max_hp: max_hp} = mobile) do
    Map.put(mobile, :hp, min(hp, max_hp))
  end

  def set_max_hp(%Mobile{} = mobile) do
    Map.put(mobile, :max_hp, trunc(strength(mobile) * (1.5 + (0.15 * mobile.level))))
  end

  def set_physical_defense(%Mobile{} = mobile) do
    Map.put(mobile, :physical_defense, physical_defense_from_items(mobile))
  end

  def set_magical_defense(%Mobile{} = mobile) do
    Map.put(mobile, :magical_defense, magical_defense_from_items(mobile))
  end

  def physical_defense_from_items(%Mobile{spirit: nil}), do: 0
  def physical_defense_from_items(%Mobile{spirit: %Spirit{equipment: equipment}}) do
    Enum.reduce(equipment, 0, fn(item, total) ->
      total + item["physical_defense"]
    end)
  end

  def magical_defense_from_items(%Mobile{spirit: nil}), do: 0
  def magical_defense_from_items(%Mobile{spirit: %Spirit{equipment: equipment}}) do
    Enum.reduce(equipment, 0, fn(item, total) ->
      total + item["magical_defense"]
    end)
  end

  def weapon(%Mobile{spirit: nil}), do: nil
  def weapon(%Mobile{spirit: %Spirit{equipment: equipment}}) do
    equipment
    |> Enum.find(fn(%{"worn_on" => worn_on}) ->
         worn_on in ["Weapon Hand", "Two Handed"]
       end)
  end

  def physical_damage_from_weapon(nil), do: 0
  def physical_damage_from_weapon(%{"strength" => str, "agility" => agi, "will" => will}) do
    physical_damage(str, agi, will)
  end

  def magical_damage_from_weapon(nil), do: 0
  def magical_damage_from_weapon(%{"strength" => str, "agility" => agi, "will" => will}) do
    magical_damage(str, agi, will)
  end

  def physical_damage(%Mobile{} = mobile) do
    str = strength(mobile)
    agi = agility(mobile)
    wil = will(mobile)

    damage =
      (physical_damage(str, agi, wil) / 3) + physical_damage_from_weapon(weapon(mobile))

    max(trunc(damage), 1)
  end

  def physical_damage(str, agi, wil) do
    total = str + agi + wil

    total * (str / total)
  end

  def magical_damage(%Mobile{} = mobile) do
    str = strength(mobile)
    agi = agility(mobile)
    wil = will(mobile)

    damage =
      (magical_damage(str, agi, wil) / 3) + magical_damage_from_weapon(weapon(mobile))

    max(trunc(damage), 1)
  end

  def magical_damage(str, agi, wil) do
    total = str + agi + wil

    total * (wil / total)
  end

  def strength(%Mobile{} = mobile) do
    attribute(mobile, :strength)
  end

  def agility(%Mobile{} = mobile) do
    attribute(mobile, :agility)
  end

  def will(%Mobile{} = mobile) do
    attribute(mobile, :will)
  end

  def attribute(%Mobile{level: level} = mobile, attribute) do
    Map.get(mobile, attribute) +
    (level * Map.get(mobile, :"#{attribute}_per_level")) +
    attribute_from_equipment(mobile, attribute)
  end

  def attribute_from_equipment(%Mobile{spirit: nil}, _), do: 0
  def attribute_from_equipment(%Mobile{spirit: %Spirit{equipment: equipment}}, attribute) do
    Enum.reduce(equipment, 0, &(&2 + &1[Atom.to_string(attribute)]))
  end

  def hp_regen_per_second(%Mobile{max_hp: max_hp} = mobile) do
    modifier = 1 + effect_bonus(mobile, "hp_regen") / 100

    max_hp * 0.01 * modifier
  end

  def mana_regen_per_second(%Mobile{max_mana: max_mana} = mobile) do
    modifier = 1 + effect_bonus(mobile, "mana_regen") / 100

    max_mana * 0.01 * modifier
  end

  def local_hated_targets(%Mobile{hate: hate} = mobile) do
    mobile
    |> Room.mobiles
    |> Enum.reduce(%{}, fn(potential_target, targets) ->
         threat = Map.get(hate, potential_target, 0)
         if threat > 0 do
           Map.put(targets, threat, potential_target)
         else
           targets
         end
       end)
  end

  def global_hated_targets(%Mobile{hate: hate}) do
    hate
    |> Map.keys
    |> Enum.reduce(%{}, fn(potential_target, targets) ->
         threat = Map.get(hate, potential_target, 0)
         if threat > 0 do
           Map.put(targets, threat, potential_target)
         else
           targets
         end
       end)
  end

  def aggro_target(%Mobile{} = mobile) do
    targets = local_hated_targets(mobile)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def most_hated_target(%Mobile{} = mobile) do
    targets = global_hated_targets(mobile)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def top_threat([]),      do: nil
  def top_threat(targets), do: Enum.max(targets)

  def handle_call(:attackable?, _from, %Mobile{monster_template_id: nil} = mobile) do
    if ethereal?(mobile) do
      {:reply, {:error, "#{mobile.name} has no body to attack."}, mobile}
    else
      {:reply, :ok, mobile}
    end
  end
  def handle_call(:attackable?, _from, %Mobile{monster_template_id: _} = mobile) do
    {:reply, :ok, mobile}
  end

  def handle_call(:able_to_possess?, _from, %Mobile{monster_template_id: nil} = mobile) do
    if ethereal?(mobile) do
      {:reply, :ok, mobile}
    else
      {:reply, {:error, "You cannot use possession if you have a physical body."}, mobile}
    end
  end
  def handle_call(:able_to_possess?, _from, %Mobile{monster_template_id: _, effects: _effects} = mobile) do
    {:reply, {:error, "You are already possessing #{mobile.name}."}, mobile}
  end

  def handle_call({:attack, target}, _from, mobile) do
    if ethereal?(mobile) do
      {:reply, {:error, "You need a physical form to attack monsters."}, mobile}
    else
      mobile =
        mobile
        |> set_attack_target(target)
        |> initiate_combat

      {:reply, :ok, mobile}
    end
  end

  def handle_call(:unpossess, _from, %Mobile{monster_template_id: nil} = mobile) do
    {:reply, {:error, "You aren't possessing anything."}, mobile}
  end
  def handle_call(:unpossess, _from, %Mobile{socket: _socket, spirit: spirit} = mobile) do
    mobile =
      mobile
      |> Map.put(:spirit, nil)
      |> Map.put(:socket, nil)

      ApathyDrive.PubSub.unsubscribe(self, "spirits:online")
      ApathyDrive.PubSub.unsubscribe(self, "spirits:#{spirit.id}")
      ApathyDrive.PubSub.unsubscribe(self, "chat:gossip")
      ApathyDrive.PubSub.unsubscribe(self, "chat:#{String.downcase(spirit.class.name)}")

    {:reply, {:ok, spirit: spirit, mobile_name: mobile.name}, mobile}
  end

  def handle_call({:possess, _spirit_id, _socket}, _from, %Mobile{monster_template_id: nil} = mobile) do
    {:reply, {:error, "You can't possess other players."}, mobile}
  end
  def handle_call({:possess, spirit_id, socket}, _from, %Mobile{spirit: nil} = mobile) do

    spirit =
      Repo.get!(Spirit, spirit_id)
      |> Repo.preload(:class)

    ApathyDrive.PubSub.subscribe(self, "spirits:online")
    ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
    ApathyDrive.PubSub.subscribe(self, "chat:gossip")
    ApathyDrive.PubSub.subscribe(self, "chat:#{String.downcase(spirit.class.name)}")

    mobile =
      mobile
      |> Map.put(:spirit, spirit)
      |> Map.put(:socket, socket)

    send(socket, {:update_mobile, self})

    send_scroll(mobile, "<p>You possess #{mobile.name}.")

    Process.monitor(socket)

    update_prompt(mobile)

    {:reply, :ok, mobile}
  end
  def handle_call({:possess, _spirit_id, _socket}, _from, mobile) do
    {:reply, {:error, "#{mobile.name} is possessed by another player."}, mobile}
  end

  def handle_call(:score_data, _from, mobile) do
    score_data(mobile)

    {:reply, score_data(mobile), mobile}
  end

  def handle_call({:look_at_item, item}, _from, mobile) do
    {:reply, look_at_item(mobile, item), mobile}
  end

  def handle_call({:get_item, %{"weight" => weight} = item}, _from, %Mobile{spirit: %Spirit{inventory: inventory}, monster_template_id: nil} = mobile) do
    if remaining_encumbrance(mobile) >= weight do
      mobile =
        put_in(mobile.spirit.inventory, [item | inventory])
        |> set_highest_armour_grade
        |> set_abilities

        Repo.update!(mobile.spirit)

      {:reply, :ok, mobile}
    else
      {:reply, :too_heavy, mobile}
    end
  end
  def handle_call({:get_item, %{"weight" => _weight} = _item}, _from, %Mobile{monster_template_id: _} = mobile) do
    {:reply, :possessed, mobile}
  end

  def handle_call({:destroy_item, item}, _from, %Mobile{spirit: %Spirit{inventory: inventory}, monster_template_id: nil} = mobile) do
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Systems.Match.one(:name_contains, item)

    case item do
      nil ->
        {:reply, :not_found, mobile}
      %{item: item} ->
        mobile =
          put_in(mobile.spirit.inventory, List.delete(inventory, item))
          |> set_highest_armour_grade
          |> set_abilities

          Repo.update!(mobile.spirit)

        {:reply, {:ok, item}, mobile}
    end
  end

  def handle_call({:drop_item, item}, _from, %Mobile{spirit: %Spirit{inventory: inventory}, monster_template_id: nil} = mobile) do
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Systems.Match.one(:name_contains, item)

    case item do
      nil ->
        {:reply, :not_found, mobile}
      %{item: item} ->
        mobile =
          put_in(mobile.spirit.inventory, List.delete(inventory, item))
          |> set_highest_armour_grade
          |> set_abilities

          Repo.update!(mobile.spirit)

        {:reply, {:ok, item}, mobile}
    end
  end
  def handle_call({:drop_item, _item}, _from, %Mobile{monster_template_id: _} = mobile) do
    {:reply, :possessed, mobile}
  end

  def handle_call({:equip_item, item}, _from, %Mobile{spirit: %Spirit{inventory: inventory, equipment: _equipment}} = mobile) do
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Systems.Match.one(:name_contains, item)

    case item do
      nil ->
       {:reply, :not_found, mobile}
     %{item: item} ->
       {:reply, resp, mobile} = equip_item(mobile, item)

       Repo.update!(mobile.spirit)

       {:reply, resp, mobile}
    end
  end

  def handle_call({:unequip_item, item}, _from, %Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile) do
    item = equipment
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Systems.Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        {:reply, :not_found, mobile}
      %{item: item_to_remove} ->
        equipment =
          equipment
          |> List.delete(item_to_remove)

        inventory =
          inventory
          |> List.insert_at(-1, item_to_remove)

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> set_highest_armour_grade
                   |> set_abilities
                   |> set_max_mana
                   |> set_mana
                   |> set_max_hp
                   |> set_hp
                   |> set_physical_defense
                   |> set_magical_defense

          Repo.update!(mobile.spirit)

        {:reply, {:ok, %{unequipped: item_to_remove}}, mobile}
    end
  end

  def handle_call({:find_item, item}, _from, %Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile) do
    item = (inventory ++ equipment)
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Systems.Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        {:reply, nil, mobile}
      %{item: item} ->
        {:reply, item, mobile}
    end
  end

  def handle_call(:data_for_who_list, _from, mobile) do
    data = %{name: mobile.spirit.name, possessing: "", class: mobile.spirit.class.name, alignment: mobile.spirit.class.alignment}

    {:reply, data, mobile}
  end

  def handle_call(:ability_list, _from, mobile) do
    abilities =
      mobile.abilities
      |> Enum.reject(&(Map.get(&1, "command") == nil))
      |> Enum.uniq(&(Map.get(&1, "command")))
      |> Enum.sort_by(&(Map.get(&1, "level")))

    {:reply, abilities, mobile}
  end

  def handle_call(:room_id, _from, mobile) do
    {:reply, mobile.room_id, mobile}
  end

  def handle_call(:greeting, _from, mobile) do
    {:reply, mobile.greeting, mobile}
  end

  def handle_call(:spirit_id, _from, mobile) do
    {:reply, mobile.spirit && mobile.spirit.id, mobile}
  end

  def handle_call(:look_name, _from, mobile) do
    {:reply, "<span class='#{alignment_color(mobile)}'>#{mobile.name}</span>", mobile}
  end

  def handle_call(:look_data, _from, mobile) do
    hp_percentage = round(100 * (mobile.hp / mobile.max_hp))

    hp_description = case hp_percentage do
      _ when hp_percentage >= 100 ->
        "unwounded"
      _ when hp_percentage >= 90 ->
        "slightly wounded"
      _ when hp_percentage >= 60 ->
        "moderately wounded"
      _ when hp_percentage >= 40 ->
        "heavily wounded"
      _ when hp_percentage >= 20 ->
        "severely wounded"
      _ when hp_percentage >= 10 ->
        "critically wounded"
      _ ->
        "very critically wounded"
    end

    hp_description =
      "{{target:He/She/It}} appears to be #{hp_description}."
      |> interpolate(%{"target" => mobile})

    data = %{
      name: mobile.name,
      description: mobile.description,
      hp_description: hp_description
    }

    {:reply, data, mobile}
  end

  def handle_call(:match_data, _from, mobile) do
    {:reply, %{name: mobile.name, keywords: mobile.keywords}, mobile}
  end

  def handle_call(:name, _from, mobile) do
    {:reply, mobile.name, mobile}
  end

  def handle_call(:blind?, _from, mobile) do
    blind =
      mobile.effects
      |> Map.values
      |> Enum.any?(&(Map.has_key?(&1, "blinded")))

    {:reply, blind, mobile}
  end

  def handle_call(:confused?, _from, mobile) do
    {:reply, confused(mobile), mobile}
  end

  def handle_call(:silenced?, _from, mobile) do
    {:reply, silenced(mobile), mobile}
  end

  def handle_call(:held?, _from, mobile) do
    {:reply, held(mobile), mobile}
  end

  def handle_call(:enter_message, _from, mobile) do
    {:reply, mobile.enter_message, mobile}
  end

  def handle_call(:exit_message, _from, mobile) do
    {:reply, mobile.exit_message, mobile}
  end

  def handle_call(:interpolation_data, _from, mobile) do
    {:reply, interpolation_data(mobile), mobile}
  end

  def handle_call(:aligned_spirit_name, _from, mobile) do
    {:reply, aligned_spirit_name(mobile), mobile}
  end

  def handle_call(:display_inventory, _from, mobile) do
    {:reply, display_inventory(mobile), mobile}
  end

  def handle_call(:value, _from, mobile) do
    {:reply, mobile, mobile}
  end

  def handle_cast({:use_ability, command, args}, mobile) do

    ability = mobile.abilities
              |> Enum.find(fn(ability) ->
                   ability["command"] == String.downcase(command)
                 end)

    if ability do
      mobile = Ability.execute(mobile, ability, Enum.join(args, " "))
      {:noreply, mobile}
    else
      Mobile.send_scroll(mobile, "<p>What?</p>")
      {:noreply, mobile}
    end
  end

  def handle_cast({:add_experience, exp}, %Mobile{spirit: spirit} = mobile) do
    new_spirit =
      spirit
      |> Spirit.add_experience(exp)

    if new_spirit.level > spirit.level do
      mobile = mobile
               |> Map.put(:spirit, new_spirit)
               |> set_abilities

      send_scroll(mobile, "<p>You've advanced to level #{new_spirit.level}!</p>")

      {:noreply, mobile}
    else
      mobile =
        mobile
        |> Map.put(:spirit, new_spirit)

      {:noreply, mobile}
    end
  end

  def handle_cast({:add_form, %{"id" => item_id, "name" => name}}, %Mobile{spirit: spirit} = mobile) do
    alias ApathyDrive.SpiritItemRecipe

    form =
      %SpiritItemRecipe{}
      |> SpiritItemRecipe.changeset(%{item_id: item_id, spirit_id: spirit.id})

    case Repo.insert(form) do
      {:ok, _recipe} ->
        Mobile.send_scroll(mobile, "<p>You gain knowledge of #{name}'s <span class='green'>form</span>, allowing you to <span class='green'>construct</span> it from raw essence.</p>")
      {:error, _} ->
        :noop
    end

    {:noreply, mobile}
  end

  def handle_cast(:display_experience, %Mobile{spirit: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_cast(:display_experience, %Mobile{spirit: spirit} = mobile) do
    Mobile.send_scroll(mobile, Commands.Experience.message(spirit))

    {:noreply, mobile}
  end

  def handle_cast({:class_chat, _message}, %Mobile{spirit: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_cast({:class_chat, message}, %Mobile{spirit: spirit} = mobile) do
    class_name = String.downcase(spirit.class.name)

    ApathyDrive.PubSub.broadcast!("chat:#{class_name}", {String.to_atom(class_name), Mobile.aligned_spirit_name(mobile), message})
    {:noreply, mobile}
  end

  def handle_info({:execute_ability, ability, arg_string}, mobile) do
    mobile = Ability.execute(mobile, ability, arg_string)
    {:noreply, mobile}
  end

  def handle_info(:display_prompt, %Mobile{socket: _socket} = mobile) do
    display_prompt(mobile)

    {:noreply, mobile}
  end

  def handle_info({:send_scroll, message}, mobile) do
    send_scroll(mobile, message)

    {:noreply, mobile}
  end

  def handle_info({:move_to, room_id}, mobile) do
    ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles")
    ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")
    mobile = Map.put(mobile, :room_id, room_id)
    ApathyDrive.PubSub.subscribe(self, "rooms:#{room_id}:mobiles")
    ApathyDrive.PubSub.subscribe(self, "rooms:#{room_id}:mobiles:#{mobile.alignment}")

    if mobile.spirit do
      mobile = put_in(mobile.spirit.room_id, mobile.room_id)
      Spirit.save(mobile.spirit)
      {:noreply, mobile}
    else
      {:noreply, mobile}
    end
  end

  def handle_info(%Phoenix.Socket.Broadcast{}, %Mobile{socket: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_info(%Phoenix.Socket.Broadcast{} = message, %Mobile{socket: socket} = mobile) do
    send(socket, message)

    {:noreply, mobile}
  end

  def handle_info(:think, mobile) do
    mobile = ApathyDrive.AI.think(mobile)

    {:noreply, mobile}
  end

  def handle_info({:apply_ability, %{} = ability, %Mobile{} = ability_user}, mobile) do
    if Ability.affects_target?(mobile, ability) do
      mobile = mobile
               |> Ability.apply_ability(ability, ability_user)

      Mobile.update_prompt(mobile)

      if mobile.hp < 1 do
        {:noreply, Systems.Death.kill(mobile)}
      else
        {:noreply, mobile}
      end
    else
      message = "#{mobile.name} is not affected by that ability." |> capitalize_first
      Mobile.send_scroll(ability_user, "<p><span class='dark-cyan'>#{message}</span></p>")
      {:noreply, mobile}
    end
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Mobile{timers: timers} = mobile) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, function})

    timers = Map.put(timers, name, new_ref)

    TimerManager.execute_function(function)

    {:noreply, Map.put(mobile, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, function}}, %Mobile{timers: timers} = mobile) do
    TimerManager.execute_function(function)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(mobile, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, mobile) do
    mobile = Systems.Effect.remove(mobile, key)
    {:noreply, mobile}
  end

  def handle_info(:regen, %Mobile{hp: hp,     max_hp: max_hp,
                                  mana: mana, max_mana: max_mana} = mobile)
                                  when hp == max_hp and mana == max_mana, do: {:noreply, mobile}

  def handle_info(:regen, %Mobile{hp: hp, max_hp: max_hp, mana: mana, max_mana: max_mana} = mobile) do
    mobile = mobile
             |> Map.put(:hp,   min(  hp + hp_regen_per_second(mobile), max_hp))
             |> Map.put(:mana, min(mana + mana_regen_per_second(mobile), max_mana))

    update_prompt(mobile)

    {:noreply, mobile}
  end

  def handle_info({:mobile_died, mobile: %Mobile{}, reward: _exp}, %Mobile{spirit: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_info({:mobile_died, mobile: %Mobile{} = deceased, reward: exp}, %Mobile{spirit: %Spirit{} = spirit} = mobile) do
    message = deceased.death_message
              |> interpolate(%{"name" => deceased.name})
              |> capitalize_first

    send_scroll(mobile, "<p>#{message}</p>")

    send_scroll(mobile, "<p>You gain #{exp} experience.</p>")

    new_spirit =
      spirit
      |> Spirit.add_experience(exp)

    if new_spirit.level > spirit.level do
      mobile = mobile
               |> Map.put(:spirit, new_spirit)
               |> set_abilities

      send_scroll(mobile, "<p>You've advanced to level #{new_spirit.level}!</p>")

      {:noreply, mobile}
    else
      mobile = mobile
                |> Map.put(:spirit, new_spirit)

      {:noreply, mobile}
    end
  end

  def handle_info({:gossip, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='dark-magenta'>gossip</span> : #{name}] #{message}</p>")

    {:noreply, mobile}
  end

  def handle_info({:angel, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='white'>angel</span> : #{name}] #{message}</p>")
    {:noreply, mobile}
  end

  def handle_info({:elemental, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='dark-cyan'>elemental</span> : #{name}] #{message}</p>")
    {:noreply, mobile}
  end

  def handle_info({:demon, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='magenta'>demon</span> : #{name}] #{message}</p>")
    {:noreply, mobile}
  end

  def handle_info(:apply_periodic_effects, mobile) do

    # periodic damage
    mobile.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "damage")))
    |> Enum.each(fn(%{"damage" => damage, "effect_message" => message}) ->
         ability = %{"kind" => "attack",
                     "ignores_global_cooldown" => true,
                     "flags" => [],
                     "instant_effects" => %{"damage" => damage},
                     "cast_message"    => %{"user" => message}}

         send(self, {:apply_ability, ability, mobile})
       end)

    # # periodic heal
    # monster.effects
    # |> Map.values
    # |> Enum.filter(&(Map.has_key?(&1, "heal")))
    # |> Enum.each(fn(%{"heal" => heal}) ->
    #     ability = %Ability{kind: "heal", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"heal" => heal}}}
    #
    #     send(self, {:apply_ability, ability, monster})
    #   end)
    #
    # # periodic heal_mana
    # monster.effects
    # |> Map.values
    # |> Enum.filter(&(Map.has_key?(&1, "heal_mana")))
    # |> Enum.each(fn(%{"heal_mana" => heal}) ->
    #     ability = %Ability{kind: "heal", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"heal_mana" => heal}}}
    #
    #     send(self, {:apply_ability, ability, monster})
    #   end)

    {:noreply, mobile}
  end

  def handle_info(:execute_auto_attack, %Mobile{attack_target: nil} = mobile) do
    send_scroll(mobile, "<p><span class='dark-yellow'>*Combat Off*</span></p>")
    {:noreply, mobile}
  end
  def handle_info(:execute_auto_attack, %Mobile{attack_target: target, auto_attack_interval: interval} = mobile) do
    if Process.alive?(target) and target in PubSub.subscribers("rooms:#{mobile.room_id}:mobiles") do
      execute_auto_attack(mobile, target)

      mobile = TimerManager.call_after(mobile, {:auto_attack_timer, interval |> seconds, fn ->
        send(self, :execute_auto_attack)
      end})
      {:noreply, mobile}
    else
      mobile =
        mobile
        |> send_scroll("<p><span class='dark-yellow'>*Combat Off*</span></p>")
        |> Map.put(:attack_target, nil)

      {:noreply, mobile}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, {:shutdown, :closed}}, %Mobile{spirit: spirit, socket: socket} = mobile) when pid == socket do
    mobile =
      mobile
      |> Map.put(:spirit, nil)
      |> Map.put(:socket, nil)

      ApathyDrive.PubSub.unsubscribe(self, "spirits:online")
      ApathyDrive.PubSub.unsubscribe(self, "spirits:#{spirit.id}")
      ApathyDrive.PubSub.unsubscribe(self, "chat:gossip")
      ApathyDrive.PubSub.unsubscribe(self, "chat:#{String.downcase(spirit.class.name)}")

    {:noreply, mobile}
  end

  def handle_info(:die, mobile) do
    Systems.Death.kill(mobile)

    {:noreply, mobile}
  end

  defp execute_auto_attack(%Mobile{} = mobile, target) do
    attack =
      mobile.abilities
      |> Enum.filter(&(&1["kind"] == "auto_attack"))
      |> Enum.random

    if attack do
      attack =
        attack
        |> Map.put("ignores_global_cooldown", true)
        |> Map.put("kind", "attack")

      send(self, {:execute_ability, attack, [target]})
    end
  end

  def set_attack_target(%Mobile{attack_target: attack_target} = mobile, target) when attack_target == target do
    mobile
  end
  def set_attack_target(%Mobile{} = mobile, target) do
    Task.start fn ->
      PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [mobile.pid, target])
      |> Enum.each(&(Mobile.send_scroll(&1, "<p><span class='dark-yellow'>#{mobile.name} moves to attack #{Mobile.name(target)}.</span></p>")))

      send_scroll(target, "<p><span class='dark-yellow'>#{mobile.name} moves to attack you.</span></p>")
    end

    Map.put(mobile, :attack_target, target)
  end

  def initiate_combat(%Mobile{timers: %{auto_attack_timer: _}} = mobile) do
    mobile
  end
  def initiate_combat(%Mobile{} = mobile) do
    send(mobile.pid, :execute_auto_attack)
    mobile
  end

  defp worn_on_max(%{"worn_on" => "Finger"}), do: 2
  defp worn_on_max(%{"worn_on" => "Wrist"}),  do: 2
  defp worn_on_max(%{"worn_on" => _}),        do: 1

  defp ethereal?(%Mobile{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["ethereal"] == true))
  end

  defp conflicting_worn_on("Weapon Hand"),     do: ["Two Handed"]
  defp conflicting_worn_on("Off-Hand"),   do: ["Two Handed"]
  defp conflicting_worn_on("Two Handed"), do: ["Weapon Hand", "Off-Hand"]
  defp conflicting_worn_on(_), do: []

  defp value(pre, post) when pre > post do
    "#{post}(<span class='dark-red'>#{post - pre}</span>)"
  end
  defp value(pre, post) when pre < post do
    "#{post}(<span class='green'>+#{post - pre}</span>)"
  end
  defp value(_pre, post) do
    "#{post}"
  end

end
