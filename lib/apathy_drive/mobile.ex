defmodule ApathyDrive.Mobile do
  alias ApathyDrive.{Commands, Mobile, Repo, Item, ItemDrop, PubSub, TimerManager, Ability, Match, MobileSupervisor, RoomServer, Room, Presence, MonsterTemplate}
  use ApathyDrive.Web, :model
  import ApathyDrive.Text

  schema "mobiles" do
    belongs_to :room, Room
    belongs_to :monster_template, MonsterTemplate

    field :name,                 :string
    field :alignment,            :string
    field :enter_message,        :string,           default: "{{name}} enters from {{direction}}."
    field :exit_message,         :string,           default: "{{name}} leaves {{direction}}."
    field :death_message,        :string,           default: "{{name}} dies."
    field :description,          :string,           default: "Some temporary description."
    field :gender,               :string
    field :greeting,             :string
    field :level,                :integer,          default: 1
    field :flags,                {:array, :string}, default: []
    field :experience,           :integer
    field :auto_attack_interval, :float,            default: 4.0
    field :unities,              {:array, :string}, default: []
    field :movement,             :string
    field :spawned_at,           :integer
    field :area_spawned_in,      :integer

    field :spirit,             :any,     virtual: true
    field :socket,             :any,     virtual: true
    field :hp,                 :float,   virtual: true
    field :max_hp,             :integer, virtual: true
    field :strength,           :integer, virtual: true
    field :agility,            :integer, virtual: true
    field :will,               :integer, virtual: true
    field :mana,               :float,   virtual: true
    field :max_mana,           :integer, virtual: true
    field :effects,            :map,     virtual: true, default: %{}
    field :keywords,           :any,     virtual: true, default: []
    field :abilities,          :any,     virtual: true, default: []
    field :hate,               :map,     virtual: true, default: %{}
    field :timers,             :map,     virtual: true, default: %{}
    field :attack_target,      :any,     virtual: true
    field :combo,              :any,     virtual: true
    field :delayed,            :boolean, virtual: true, default: false
    field :last_effect_key,    :integer, virtual: true, default: 0
    field :chance_to_follow,   :integer, virtual: true, default: 0
    field :movement_frequency, :integer, virtual: true, default: 60
    field :room_essences,      :map,     virtual: true, default: %{}
    field :unity_essences,     :map,     virtual: true, default: %{}
    field :pid,                :any,     virtual: true
    field :ref,                :any,     virtual: true

    timestamps
  end

  def generate_loot(%Mobile{} = mobile, monster_template_id, level, global_chance) do
    ItemDrop.monster_drops(monster_template_id)
    |> Enum.map(fn(%{item_id: item_id, chance: chance}) ->
         ApathyDrive.Item.generate_item(%{chance: chance, item_id: item_id, level: level})
       end)
    |> List.insert_at(0, ApathyDrive.Item.generate_item(%{chance: global_chance, item_id: :global, level: level}))
    |> Enum.reject(&is_nil/1)
    |> case do
         [] ->
           mobile
         items ->
           Mobile.send_scroll(mobile, "<p>\n<span class='white'>A wild surge of spirtual essence coalesces into:</span></p>")
           Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>   STR  |   AGI  |   WIL  | Item Name</span></p>")
           mobile =
             Enum.reduce(items, mobile, fn(item, updated_mobile) ->
               current_data =
                 updated_mobile
                 |> Mobile.score_data

               equipped =
                 updated_mobile
                 |> Commands.Wear.equip_item(item)

               equipped_data = Mobile.score_data(equipped.mobile)

               data =
                 current_data
                 |> Map.take([:strength, :agility, :will])
                 |> Enum.reduce(%{}, fn({key, val}, values) ->
                      diff = equipped_data[key] - val
                      color = cond do
                        diff > 0 ->
                          "green"
                        diff < 0 ->
                          "dark-red"
                        true ->
                          "dark-cyan"
                      end
                      value =
                        apply(Item, key, [item])
                        |> to_string
                        |> String.rjust(3)

                      diff =
                        String.ljust("(#{diff})", 5)

                      Map.put(values, key, "#{value}<span class='#{color}'>#{diff}</span>")
                    end)

               Mobile.send_scroll(updated_mobile, "<p><span class='dark-cyan'>#{data.strength}|#{data.agility}|#{data.will}| #{item["name"]}</span></p>")

               if (equipped_data.strength + equipped_data.agility + equipped_data.will) > (current_data.strength + current_data.agility + current_data.will) do
                 if Map.has_key?(equipped, :unequipped) do
                   Enum.each(equipped.unequipped, fn(unequipped_item) ->
                     Mobile.send_scroll(updated_mobile, "<p>You remove #{unequipped_item["name"]}.</p>")
                   end)
                 end

                 Mobile.send_scroll(updated_mobile, "<p>You are now wearing #{item["name"]}.</p>")

                 if Map.has_key?(equipped, :unequipped) do
                   Enum.reduce(equipped.unequipped, equipped.mobile, fn(unequipped_item, unequipped_mobile) ->
                      exp = ApathyDrive.Item.deconstruction_experience(unequipped_item)
                      Mobile.send_scroll(unequipped_mobile, "<p>You disintegrate the #{unequipped_item["name"]} and absorb #{exp} essence.</p>")

                      put_in(unequipped_mobile.spirit.inventory, List.delete(unequipped_mobile.spirit.inventory, unequipped_item))
                      |> Mobile.add_experience(exp)
                   end)
                 else
                   equipped.mobile
                 end
               else
                 exp = ApathyDrive.Item.deconstruction_experience(item)
                 Mobile.send_scroll(updated_mobile, "<p>You disintegrate the #{item["name"]} and absorb #{exp} essence.</p>")
                 Mobile.add_experience(updated_mobile, exp)
               end
             end)

           Mobile.save(mobile)
       end
  end

  def set_room_id(%Mobile{spirit: nil} = mobile, room_id) do
    put_in(mobile.room_id, room_id)
    |> save
  end
  def set_room_id(%Mobile{} = mobile, room_id) do
    send(mobile.socket, {:update_room, room_id})
    mobile = put_in(mobile.spirit.room_id, room_id)
    put_in(mobile.room_id, room_id)
    |> save
  end

  def cancel_timer(%Mobile{} = mobile, timer) do
    update_in(mobile.timers, &Map.delete(&1, timer))
  end

  def ids do
    __MODULE__
    |> distinct(true)
    |> select([m], %{id: m.id, room_id: m.room_id, monster_template_id: m.monster_template_id})
  end

  def body_required(mobile) do
    Mobile.send_scroll(mobile, "<p>You need a body to do that. Find a monster and <span class='green'>possess</span> it.</p>")
  end

  def remonitor(%Mobile{spirit: nil} = mobile), do: mobile
  def remonitor(%Mobile{socket: socket, spirit: %Spirit{monitor_ref: ref}} = mobile) do
    Process.demonitor(ref)
    put_in(mobile.spirit.monitor_ref, Process.monitor(socket))
  end

  def add_experience(%Mobile{experience: experience, level: level} = mobile, exp) do
    initial_spirit_level = mobile.spirit && mobile.spirit.level

    mobile = Spirit.add_experience(mobile, exp)

    mobile =
      mobile
      |> Map.put(:experience, (mobile.spirit && mobile.spirit.experience) || experience + exp)
      |> ApathyDrive.Level.advance

    if (mobile.level != level) or (initial_spirit_level != (mobile.spirit && mobile.spirit.level)) do
      mobile
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
    else
      mobile
    end
  end

  def sanitize(message) do
    {:safe, message} = Phoenix.HTML.html_escape(message)

    message
  end

  def score_data(mobile) do
    effects =
      mobile.effects
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "effect_message")))
      |> Enum.map(&(&1["effect_message"]))

    %{name: mobile.spirit.name,
      class: (mobile.monster_template_id && mobile.name) || mobile.spirit.class.name,
      level: level(mobile),
      experience: mobile.spirit.experience,
      hp: mobile.hp,
      max_hp: mobile.max_hp,
      mana: mobile.mana,
      max_mana: mobile.max_mana,
      strength: strength(mobile),
      agility: agility(mobile),
      will: will(mobile),
      effects: effects,
      physical_defense: (1 - reduce_damage(mobile, "physical defense")) * 100,
      magical_defense:  (1 - reduce_damage(mobile, "magical defense")) * 100,
      physical_damage: physical_damage(mobile),
      magical_damage: magical_damage(mobile)}
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

  def update_prompt(%Mobile{socket: nil}), do: :noop
  def update_prompt(%Mobile{socket: socket} = mobile) do
    send(socket, {:update_prompt, prompt(mobile)})
  end

  def prompt(%Mobile{monster_template_id: nil} = mobile) do
    "[ES=#{trunc(mobile.spirit.experience)}]:"
  end
  def prompt(%Mobile{} = mobile) do
    cond do
      mobile.hp / mobile.max_hp > 0.5 ->
        "[HP=#{trunc(mobile.hp)}/MA=#{trunc(mobile.mana)}]:"
      mobile.hp / mobile.max_hp > 0.20 ->
        "[HP=<span class='dark-red'>#{trunc(mobile.hp)}</span>/MA=#{trunc(mobile.mana)}]:"
      true ->
        "[HP=<span class='red'>#{trunc(mobile.hp)}</span>/MA=#{trunc(mobile.mana)}]:"
    end
  end

  def alignment_color(%{unities: ["evil"]}), do: "magenta"
  def alignment_color(%{unities: ["good"]}), do: "white"
  def alignment_color(%{unities: _}),        do: "dark-cyan"

  def evil_points(%{alignment: "evil"}),    do: 250
  def evil_points(%{alignment: "good"}),    do: -215
  def evil_points(%{alignment: "neutral"}), do: 0

  def has_item?(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}}, item_template_id) do
    (inventory ++ equipment)
    |> Enum.map(&Map.get(&1, "id"))
    |> Enum.member?(item_template_id)
  end

  def remove_item?(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, item_template_id) do
    inventory_item =
      inventory
      |> Enum.find(&(&1["id"] == item_template_id))

    if inventory_item do
      put_in(mobile.spirit.inventory, List.delete(inventory, inventory_item))
    else
      equipment_item =
        equipment
        |> Enum.find(&(&1["id"] == item_template_id))

      if equipment_item do
        put_in(mobile.spirit.equipment, List.delete(equipment, equipment_item))
      end
    end
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

  def confused(%Mobile{effects: effects} = mobile) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "confused") && (effect["confused"] >= :rand.uniform(100))
       end)
    |> confused(mobile)
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
  def reduce_damage(%Mobile{} = mobile, mitigator) do
    1 - (0.01 * Mobile.effect_bonus(mobile, mitigator))
  end

  def reduce_damage(%Mobile{} = mobile, damage, nil), do: reduce_damage(mobile, damage, [])
  def reduce_damage(%Mobile{} = mobile, damage, mitigated_by) when is_list(mitigated_by) do
    multiplier = Enum.reduce(["damage resistance" | mitigated_by], 1, fn(mitigating_factor, multiplier) ->
      multiplier * reduce_damage(mobile, mitigating_factor)
    end)

    max(0, trunc(damage * multiplier))
  end

  def physical_defense(%Mobile{} = mobile) do
    (physical_damage(mobile) * 2) * (4 + 0.01 * level(mobile)) + effect_bonus(mobile, "physical defense")
  end

  def magical_defense(%Mobile{} = mobile) do
    (magical_damage(mobile) * 2) * (4 + 0.01 * level(mobile)) + effect_bonus(mobile, "magical defense")
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

  def send_scroll(%Mobile{socket: nil} = mobile, _html),  do: mobile
  def send_scroll(%Mobile{socket: socket} = mobile, html) do
    send(socket, {:scroll, html})
    mobile
  end

  def init(%Mobile{spirit: nil} = mobile) do
    ref = make_ref()

    mobile =
      mobile
      |> Map.put(:ref, ref)
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> TimerManager.send_after({:monster_regen,    1_000, {:regen, ref}})
      |> TimerManager.send_after({:periodic_effects, 3_000, {:apply_periodic_effects, ref}})
      |> TimerManager.send_after({:monster_ai,       5_000, {:think, ref}})
      |> TimerManager.send_after({:unify, 60_000, {:unify, ref}})

      save(mobile)
  end
  def init(%Mobile{spirit: spirit} = mobile) do
    ref = make_ref()

    mobile =
      mobile
      |> Map.put(:spirit, spirit)
      |> Map.put(:ref, ref)
      |> Map.put(:room_id, spirit.room_id)
      |> Map.put(:alignment, spirit.class.alignment)
      |> Map.put(:name, spirit.name)
      |> Map.put(:experience, spirit.experience)
      |> Map.put(:level, spirit.level)
      |> Map.put(:unities, spirit.class.unities)
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> TimerManager.send_after({:unify, 60_000, {:unify, ref}})

    save(mobile)
  end

  def set_attack_target(%Mobile{attack_target: attack_target} = mobile, target) when attack_target == target do
    mobile
  end
  def set_attack_target(%Mobile{} = mobile, target) do
    Map.put(mobile, :attack_target, target)
  end

  def initiate_combat(%Mobile{timers: %{auto_attack_timer: _}} = mobile) do
    mobile
  end
  def initiate_combat(%Mobile{} = mobile) do
    send(self, {:execute_auto_attack, mobile.ref})
    mobile
  end

  def set_abilities(%Mobile{monster_template_id: nil, spirit: _spirit} = mobile) do
    mobile
    |> Map.put(:abilities, [])
  end
  def set_abilities(%Mobile{monster_template_id: mt_id, spirit: nil} = mobile) do
    abilities = MonsterTemplate.abilities(mt_id)

    mobile
    |> Map.put(:abilities, abilities)
    |> set_passive_effects
    |> adjust_mana_costs
  end
  def set_abilities(%Mobile{monster_template_id: mt_id, spirit: spirit} = mobile) do
    spirit_abilities =
     ApathyDrive.ClassAbility.for_spirit(spirit)
     |> add_abilities_from_equipment(spirit.equipment)

    monster_abilities = MonsterTemplate.abilities(mt_id)

    mobile
    |> Map.put(:abilities, monster_abilities ++ spirit_abilities)
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


    new_passive_names = Enum.map(new_passives, &(Map.get(&1, "name")))
    passives_to_remove = original_passives -- new_passive_names
    passive_names_to_add = new_passive_names -- original_passives

    mobile = remove_passive_effects(mobile, passives_to_remove)

    passives_to_add =
      Enum.reduce(passive_names_to_add, [], fn(passive_name, to_add) ->
        [Enum.find(new_passives, &(&1["name"] == passive_name)) | to_add]
      end)

    mobile = passives_to_add
    |> Enum.reduce(mobile, fn(%{"name" => name, "passive_effects" => effect}, new_mobile) ->
         Systems.Effect.add_effect(new_mobile, name, effect)
       end)

    mobile
  end

  def remove_passive_effects(%Mobile{} = mobile, effect_keys_to_remove) do
    Enum.reduce(effect_keys_to_remove, mobile, fn(effect_key, new_mobile) ->
      Systems.Effect.remove(new_mobile, effect_key, show_expiration_message: true)
    end)
  end

  def passive_effects(%Mobile{effects: effects}) do
    effects
    |> Map.keys
    |> Enum.filter(&(String.starts_with?(to_string(&1), "passive")))
  end

  def adjust_mana_costs(%Mobile{monster_template_id: nil} = mobile), do: mobile
  def adjust_mana_costs(%Mobile{} = mobile) do
    abilities =
      mobile.abilities
      |> Enum.map(&(adjust_mana_cost(mobile, &1)))

    Map.put(mobile, :abilities, abilities)
  end
  def adjust_mana_cost(%Mobile{} = _mobile, %{"mana_cost" => base} = ability) do
    Map.put(ability, "mana_cost",  base) #trunc(base + base * ((level(mobile) * 0.1) * ((level(mobile) * 0.1)))))
  end
  def adjust_mana_cost(%Mobile{}, %{} = ability), do: ability

  def set_mana(%Mobile{monster_template_id: nil} = mobile), do: mobile
  def set_mana(%Mobile{mana: nil, max_mana: max_mana} = mobile) do
    Map.put(mobile, :mana, max_mana)
  end
  def set_mana(%Mobile{mana: mana, max_mana: max_mana} = mobile) do
    Map.put(mobile, :mana, min(mana, max_mana))
  end

  def set_max_mana(%Mobile{monster_template_id: nil} = mobile), do: mobile
  def set_max_mana(%Mobile{} = mobile) do
    attr = div((will(mobile) * 2) + agility(mobile), 3)
    Map.put(mobile, :max_mana, trunc(attr * (0.18 + (0.018 * level(mobile)))))
  end

  def set_hp(%Mobile{monster_template_id: nil} = mobile), do: mobile
  def set_hp(%Mobile{hp: nil, max_hp: max_hp} = mobile) do
    Map.put(mobile, :hp, max_hp)
  end
  def set_hp(%Mobile{hp: hp, max_hp: max_hp} = mobile) do
    Map.put(mobile, :hp, min(hp, max_hp))
  end

  def set_max_hp(%Mobile{monster_template_id: nil} = mobile), do: mobile
  def set_max_hp(%Mobile{} = mobile) do
    attr = div((strength(mobile) * 2) + agility(mobile), 3)
    Map.put(mobile, :max_hp, trunc(attr * (0.6 + (0.06 * level(mobile)))))
  end

  def level(%Mobile{level: level}), do: level

  def weapon(%Mobile{spirit: nil}), do: nil
  def weapon(%Mobile{spirit: %Spirit{equipment: equipment}}) do
    equipment
    |> Enum.find(fn(%{"worn_on" => worn_on}) ->
         worn_on in ["Weapon Hand", "Two Handed"]
       end)
  end

  def physical_damage(%Mobile{} = mobile) do
    str = strength(mobile)
    agi = agility(mobile)
    wil = will(mobile)

    physical_damage(str, agi, wil)
  end

  def physical_damage(str, agi, _wil) do
    ((str * 2) + agi) / 20
  end

  def magical_damage(%Mobile{} = mobile) do
    str = strength(mobile)
    agi = agility(mobile)
    wil = will(mobile)

    magical_damage(str, agi, wil)
  end

  def magical_damage(_str, agi, wil) do
    ((wil * 2) + agi) / 20
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

  def attribute(%Mobile{spirit: nil, strength: nil, agility: nil, will: nil, level: level}, _attribute) do
    50 + (10 * level)
  end

  def attribute(%Mobile{spirit: nil, strength: str}, :strength), do: str
  def attribute(%Mobile{spirit: nil, agility:  agi}, :agility),  do: agi
  def attribute(%Mobile{spirit: nil, will:    will}, :will),     do: will

  def attribute(%Mobile{monster_template_id: nil} = mobile, attribute) do
    attribute_from_equipment(mobile, attribute)
  end

  def attribute(%Mobile{} = mobile, attribute) do
    mobile_attribute =
      mobile
      |> Map.put(:spirit, nil)
      |> attribute(attribute)

    attribute_from_equipment(mobile, attribute) + mobile_attribute
  end

  def attribute_from_equipment(%Mobile{spirit: nil}, _), do: 0
  def attribute_from_equipment(%Mobile{spirit: %Spirit{equipment: equipment}}, attribute) do
    Enum.reduce(equipment, 0, &(&2 + apply(ApathyDrive.Item, attribute, [&1])))
  end

  def hp_regen_per_second(%Mobile{max_hp: max_hp} = mobile) do
    modifier = 1 + effect_bonus(mobile, "hp_regen") / 100

    normal_regen = max_hp * 0.01 * modifier

    poison = effect_bonus(mobile, "poison") / 10

    normal_regen - poison
  end

  def mana_regen_per_second(%Mobile{max_mana: max_mana} = mobile) do
    modifier = 1 + effect_bonus(mobile, "mana_regen") / 100

    max_mana * 0.01 * modifier
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

  def aggro_target(%Room{} = room, %Mobile{} = mobile) do
    targets = Room.local_hated_targets(room, mobile)

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

  def look_name(%Mobile{} = mobile, opts \\ []) do
    name =
      mobile.name
      |> String.ljust(opts[:ljust] || 0)
      |> String.rjust(opts[:rjust] || 0)

    "<span class='#{alignment_color(mobile)}'>#{name}</span>"
  end

  def track_data(%Mobile{spirit: spirit} = mobile) do
    %{
      mobile: mobile.ref,
      alignment: mobile.alignment,
      unities: mobile.unities,
      essence: mobile.experience,
      spirit_unities: spirit && spirit.class.unities,
      spirit_essence: spirit && spirit.experience,
      spawned_at: mobile.spawned_at,
      name: mobile.name,
      keywords: String.split(mobile.name),
      look_name: look_name(mobile),
      invisible?: !mobile.monster_template_id
    }
  end

  def save(%Mobile{monster_template_id: nil, spirit: spirit} = mobile) do
    Map.put(mobile, :spirit, Repo.save!(Map.put(spirit, :experience, trunc(spirit.experience))))
  end

  def save(%Mobile{monster_template_id: _, spirit: nil} = mobile) do
    Repo.save!(Map.put(mobile, :experience, trunc(mobile.experience)))
  end

  def save(%Mobile{monster_template_id: _, spirit: spirit} = mobile) do
    mobile
    |> Map.put(:experience, trunc(mobile.experience))
    |> Map.put(:spirit, Repo.save!(Map.put(spirit, :experience, trunc(spirit.experience))))
    |> Repo.save!
  end

  def update_essence(%Mobile{spirit: spirit} = mobile, %Room{} = room) do
    current_essence = (spirit && spirit.experience) || mobile.experience

    target_essence = target_essence(mobile)

    difference = target_essence - current_essence
    amount_to_shift = difference / 5 / 60 * Room.essence_update_interval(room) / 1000

    percent_difference = if current_essence == 0, do: 1, else: abs(difference) / current_essence

    mobile =
      cond do
        percent_difference == 0 or amount_to_shift == 0 ->
          mobile
        percent_difference <= 0.10 ->
          add_experience(mobile, difference)
        true ->
          add_experience(mobile, amount_to_shift)
      end

    Mobile.update_prompt(mobile)

    mobile
  end

  defp target_essence(%Mobile{spirit: spirit, room_essences: room_essences, unity_essences: unity_essences} = mobile) do
    unities = (spirit && spirit.class.unities) || mobile.unities

    essences =
      case unities do
        [] ->
          if room_essences["default"] do
            [room_essences["default"]]
          else
            []
          end
        unities ->
          unities
          |> Enum.reduce([], fn(unity, essences) ->
               essences = if room_essences[unity], do: [room_essences[unity] | essences], else: essences
               if unity_essences[unity], do: [unity_essences[unity] | essences], else: essences
             end)
      end

    if length(essences) > 0 do
      essences = [Enum.max(essences) | essences]

      Enum.sum(essences) / length(essences)
    end
  end

end
