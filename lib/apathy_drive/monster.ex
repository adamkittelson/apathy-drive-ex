defmodule ApathyDrive.Monster do
  alias ApathyDrive.{Character, Commands, Monster, Class, Repo, Item, ItemDrop, PubSub, TimerManager, Ability, Match, MonsterSupervisor, RoomServer, Room, Presence, MonsterTemplate}
  use ApathyDrive.Web, :model
  import ApathyDrive.Text
  
  schema "monsters" do
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
    field :limbs,                ApathyDrive.JSONB
    field :crippled_limbs,       {:array, :string}, default: []
    field :missing_limbs,        {:array, :string}, default: []

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
    field :base_hp,            :integer, virtual: true

    timestamps
  end

  def uncrippled_limb(%Monster{} = monster, limb_type) do
    monster.limbs
    |> Map.keys
    |> Enum.filter(fn limb_name ->
         (limb_type == "non_fatal" and !monster.limbs[limb_name]["fatal"]) or
         (limb_type == monster.limbs[limb_name]["kind"]) or
         (limb_type == limb_name)
       end)
    |> Enum.reject(&(&1 in (monster.crippled_limbs ++ monster.missing_limbs)))
    |> case do
         [] ->
           nil
         limbs ->
           Enum.random(limbs)
       end
  end

  def unsevered_limb(%Monster{} = monster, limb_type) do
    monster.limbs
    |> Map.keys
    |> Enum.filter(fn limb_name ->
         (limb_type == "non_fatal" and !monster.limbs[limb_name]["fatal"]) or
         (limb_type == monster.limbs[limb_name]["kind"]) or
         (limb_type == limb_name)
       end)
    |> Enum.reject(&(&1 in monster.missing_limbs))
    |> case do
         [] ->
           nil
         limbs ->
           Enum.random(limbs)
       end
  end

  def generate_loot(%Monster{} = monster, monster_template_id, level, global_chance) do
    ItemDrop.monster_drops(monster_template_id)
    |> Enum.map(fn(%{item_id: item_id, chance: chance}) ->
         ApathyDrive.Item.generate_item(%{chance: chance, item_id: item_id, level: level})
       end)
    |> List.insert_at(0, ApathyDrive.Item.generate_item(%{chance: global_chance, item_id: :global, level: level}))
    |> Enum.reject(&is_nil/1)
    |> case do
         [] ->
           monster
         items ->
           Monster.send_scroll(monster, "<p>\n<span class='white'>A wild surge of spirtual essence coalesces into:</span></p>")
           Monster.send_scroll(monster, "<p><span class='dark-magenta'>   STR  |   AGI  |   WIL  | Item Name</span></p>")
           monster =
             Enum.reduce(items, monster, fn(item, updated_monster) ->
               current_data =
                 updated_monster
                 |> Monster.score_data

               equipped =
                 updated_monster
                 |> Commands.Wear.equip_item(item)

               equipped_data = Monster.score_data(equipped.monster)

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

               Monster.send_scroll(updated_monster, "<p><span class='dark-cyan'>#{data.strength}|#{data.agility}|#{data.will}| #{item["name"]}</span></p>")

               if (equipped_data.strength + equipped_data.agility + equipped_data.will) > (current_data.strength + current_data.agility + current_data.will) do
                 if Map.has_key?(equipped, :unequipped) do
                   Enum.each(equipped.unequipped, fn(unequipped_item) ->
                     Monster.send_scroll(updated_monster, "<p>You remove #{unequipped_item["name"]}.</p>")
                   end)
                 end

                 Monster.send_scroll(updated_monster, "<p>You are now wearing #{item["name"]}.</p>")

                 if Map.has_key?(equipped, :unequipped) do
                   Enum.reduce(equipped.unequipped, equipped.monster, fn(unequipped_item, unequipped_monster) ->
                      exp = ApathyDrive.Item.deconstruction_experience(unequipped_item)
                      Monster.send_scroll(unequipped_monster, "<p>You disintegrate the #{unequipped_item["name"]} and absorb #{exp} essence.</p>")

                      put_in(unequipped_monster.spirit.inventory, List.delete(unequipped_monster.spirit.inventory, unequipped_item))
                      |> Monster.add_experience(exp)
                   end)
                 else
                   equipped.monster
                 end
               else
                 exp = ApathyDrive.Item.deconstruction_experience(item)
                 Monster.send_scroll(updated_monster, "<p>You disintegrate the #{item["name"]} and absorb #{exp} essence.</p>")
                 Monster.add_experience(updated_monster, exp)
               end
             end)

           Monster.save(monster)
       end
  end

  def cancel_timer(%Monster{} = monster, timer) do
    update_in(monster.timers, &Map.delete(&1, timer))
  end

  def ids do
    __MODULE__
    |> distinct(true)
    |> select([m], %{id: m.id, room_id: m.room_id, monster_template_id: m.monster_template_id})
  end

  def body_required(monster) do
    Monster.send_scroll(monster, "<p>You need a body to do that. Find a monster and <span class='green'>possess</span> it.</p>")
  end

  def remonitor(%Monster{spirit: nil} = monster), do: monster
  def remonitor(%Monster{socket: socket, spirit: %Spirit{monitor_ref: ref}} = monster) do
    Process.demonitor(ref)
    put_in(monster.spirit.monitor_ref, Process.monitor(socket))
  end

  def add_experience(%Monster{experience: experience, level: level} = monster, exp) do
    initial_spirit_level = monster.spirit && monster.spirit.level

    monster = Spirit.add_experience(monster, exp)

    monster =
      monster
      |> Map.put(:experience, (monster.spirit && monster.spirit.experience) || experience + exp)
      |> ApathyDrive.Level.advance

    if (monster.level != level) or (initial_spirit_level != (monster.spirit && monster.spirit.level)) do
      monster
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
    else
      monster
    end
  end

  def sanitize(message) do
    {:safe, message} = Phoenix.HTML.html_escape(message)

    message
  end

  def aligned_spirit_name(%Monster{spirit: %Spirit{name: name, class: %{alignment: "good"}}}) do
    "<span class='white'>#{name}</span>"
  end
  def aligned_spirit_name(%Monster{spirit: %Spirit{name: name, class: %{alignment: "neutral"}}}) do
    "<span class='dark-cyan'>#{name}</span>"
  end
  def aligned_spirit_name(%Monster{spirit: %Spirit{name: name, class: %{alignment: "evil"}}}) do
    "<span class='magenta'>#{name}</span>"
  end

  def alignment_color(%{unities: ["evil"]}), do: "magenta"
  def alignment_color(%{unities: ["good"]}), do: "white"
  def alignment_color(%{unities: _}),        do: "dark-cyan"

  def evil_points(%{alignment: "evil"}),    do: 250
  def evil_points(%{alignment: "good"}),    do: -215
  def evil_points(%{alignment: "neutral"}), do: 0

  def has_item?(%Monster{spirit: %Spirit{inventory: inventory, equipment: equipment}}, item_template_id) do
    (inventory ++ equipment)
    |> Enum.map(&Map.get(&1, "id"))
    |> Enum.member?(item_template_id)
  end

  def remove_item?(%Monster{spirit: %Spirit{inventory: inventory, equipment: equipment}} = monster, item_template_id) do
    inventory_item =
      inventory
      |> Enum.find(&(&1["id"] == item_template_id))

    if inventory_item do
      put_in(monster.spirit.inventory, List.delete(inventory, inventory_item))
    else
      equipment_item =
        equipment
        |> Enum.find(&(&1["id"] == item_template_id))

      if equipment_item do
        put_in(monster.spirit.equipment, List.delete(equipment, equipment_item))
      end
    end
  end

  def silenced(%Monster{effects: effects} = monster, %{"mana_cost" => cost}) when cost > 0 do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "silenced")
       end)
    |> silenced(monster)
  end
  def silenced(%Monster{}, %{}), do: false
  def silenced(nil, %Monster{}), do: false
  def silenced(%{"effect_message" => message}, %Monster{} = monster) do
    send_scroll(monster, "<p>#{message}</p>")
  end

  def reduce_damage(%Monster{} = monster, "physical defense") do
    1 - (0.00044 * physical_defense(monster))
  end
  def reduce_damage(%Monster{} = monster, "magical defense") do
    1 - (0.00044 * magical_defense(monster))
  end
  def reduce_damage(%Monster{} = monster, mitigator) do
    1 - (0.01 * Monster.effect_bonus(monster, mitigator))
  end

  def reduce_damage(%Monster{} = monster, damage, nil), do: reduce_damage(monster, damage, [])
  def reduce_damage(%Monster{} = monster, damage, mitigated_by) when is_list(mitigated_by) do
    multiplier = Enum.reduce(["damage resistance" | mitigated_by], 1, fn(mitigating_factor, multiplier) ->
      multiplier * reduce_damage(monster, mitigating_factor)
    end)

    max(0, trunc(damage * multiplier))
  end

  def physical_defense(%Monster{} = monster) do
    (physical_damage(monster) * 2) * (4 + 0.01 * level(monster)) + effect_bonus(monster, "physical defense")
  end

  def magical_defense(%Monster{} = monster) do
    (magical_damage(monster) * 2) * (4 + 0.01 * level(monster)) + effect_bonus(monster, "magical defense")
  end

  def effect_bonus(%Monster{effects: effects}, name) do
    effects
    |> Map.values
    |> Enum.map(fn
         (%{} = effect) ->
           key =
             effect
             |> Map.keys
             |> Enum.find(fn key ->
                  String.downcase(to_string(key)) == String.downcase(to_string(name))
                end)

           if key, do: Map.get(effect, key, 0), else: 0
         (_) ->
           0
       end)
    |> Enum.sum
  end

  def send_scroll(%Monster{socket: nil} = monster, _html),  do: monster
  def send_scroll(%Monster{socket: socket} = monster, html) do
    send(socket, {:scroll, html})
    monster
  end

  def init(%Monster{spirit: nil} = monster) do
    ref = make_ref()

    virtuals = MonsterTemplate.monster_virtual_fields(monster.monster_template_id)

    monster =
      monster
      |> Map.put(:ref, ref)
      |> Map.put(:base_hp, virtuals.base_hp)
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> TimerManager.send_after({:monster_regen,    1_000, {:regen, ref}})
      |> TimerManager.send_after({:periodic_effects, 1_000, {:apply_periodic_effects, ref}})
      |> TimerManager.send_after({:monster_ai,       5_000, {:think, ref}})
      |> TimerManager.send_after({:unify, 60_000, {:unify, ref}})

      save(monster)
  end
  def init(%Monster{spirit: spirit} = monster) do
    ref = make_ref()

    monster =
      monster
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

    save(monster)
  end

  def set_attack_target(%Monster{attack_target: attack_target} = monster, target) when attack_target == target do
    monster
  end
  def set_attack_target(%Monster{} = monster, target) do
    Map.put(monster, :attack_target, target)
  end

  def initiate_combat(%Monster{timers: %{auto_attack_timer: _}} = monster) do
    monster
  end
  def initiate_combat(%Monster{} = monster) do
    send(self, {:execute_auto_attack, monster.ref})
    monster
  end

  def set_abilities(%Monster{monster_template_id: nil, spirit: _spirit} = monster) do
    monster
    |> Map.put(:abilities, [])
  end

  def set_abilities(%Monster{spirit: nil} = monster) do
    monster
    |> set_passive_effects
    |> adjust_mana_costs
  end

  def set_abilities(%Monster{monster_template_id: id, spirit: spirit} = monster) do
    monster_abilities = MonsterTemplate.monster_virtual_fields(id).abilities

    spirit_abilities =
     spirit.class.abilities
     |> add_abilities_from_equipment(spirit.equipment)

    monster
    |> Map.put(:abilities, spirit_abilities ++ monster_abilities)
    |> set_passive_effects
    |> adjust_mana_costs
  end


  def add_abilities_from_equipment(abilities, equipment) do
    abilities ++ Enum.flat_map(equipment, &(&1["abilities"]))
  end

  def set_passive_effects(%Monster{abilities: []} = monster) do
    remove_passive_effects(monster, passive_effects(monster))
  end
  def set_passive_effects(%Monster{abilities: abilities} = monster) do
    original_passives = passive_effects(monster)

    new_passives =
      abilities
      |> Enum.filter(&(Map.has_key?(&1, "passive_effects")))
      |> Enum.map(&(%{"name" => "passive_#{&1["name"]}", "passive_effects" => &1["passive_effects"]}))


    new_passive_names = Enum.map(new_passives, &(Map.get(&1, "name")))
    passives_to_remove = original_passives -- new_passive_names
    passive_names_to_add = new_passive_names -- original_passives

    monster = remove_passive_effects(monster, passives_to_remove)

    passives_to_add =
      Enum.reduce(passive_names_to_add, [], fn(passive_name, to_add) ->
        [Enum.find(new_passives, &(&1["name"] == passive_name)) | to_add]
      end)

    monster = passives_to_add
    |> Enum.reduce(monster, fn(%{"name" => name, "passive_effects" => effect}, new_monster) ->
         Systems.Effect.add_effect(new_monster, name, effect)
       end)

    monster
  end

  def remove_passive_effects(%Monster{} = monster, effect_keys_to_remove) do
    Enum.reduce(effect_keys_to_remove, monster, fn(effect_key, new_monster) ->
      Systems.Effect.remove(new_monster, effect_key, show_expiration_message: true)
    end)
  end

  def passive_effects(%Monster{effects: effects}) do
    effects
    |> Map.keys
    |> Enum.filter(&(String.starts_with?(to_string(&1), "passive")))
  end

  def adjust_mana_costs(%Monster{monster_template_id: nil} = monster), do: monster
  def adjust_mana_costs(%Monster{} = monster) do
    abilities =
      monster.abilities
      |> Enum.map(&(adjust_mana_cost(monster, &1)))

    Map.put(monster, :abilities, abilities)
  end
  def adjust_mana_cost(%Monster{} = _monster, %{"mana_cost" => base} = ability) do
    Map.put(ability, "mana_cost",  base) #trunc(base + base * ((level(monster) * 0.1) * ((level(monster) * 0.1)))))
  end
  def adjust_mana_cost(%Monster{}, %{} = ability), do: ability

  def set_mana(%Monster{monster_template_id: nil} = monster), do: monster
  def set_mana(%Monster{mana: nil, max_mana: max_mana} = monster) do
    Map.put(monster, :mana, max_mana)
  end
  def set_mana(%Monster{mana: mana, max_mana: max_mana} = monster) do
    Map.put(monster, :mana, min(mana, max_mana))
  end

  def set_max_mana(%Monster{monster_template_id: nil} = monster), do: monster
  def set_max_mana(%Monster{} = monster) do
    attr = div((will(monster) * 2) + agility(monster), 3)
    Map.put(monster, :max_mana, trunc(attr * (0.18 + (0.018 * level(monster)))))
  end

  def set_hp(%Monster{monster_template_id: nil} = monster), do: monster
  def set_hp(%Monster{hp: nil, max_hp: max_hp} = monster) do
    Map.put(monster, :hp, max_hp)
  end
  def set_hp(%Monster{hp: hp, max_hp: max_hp} = monster) do
    Map.put(monster, :hp, min(hp, max_hp))
  end

  def set_max_hp(%Monster{monster_template_id: nil} = monster), do: monster
  def set_max_hp(%Monster{base_hp: base} = monster) do
    attr = div((strength(monster) * 2) + agility(monster), 3)
    Map.put(monster, :max_hp, base + trunc(attr * (0.6 + (0.06 * level(monster)))))
  end

  def level(%Monster{level: level}), do: level

  def weapon(%Monster{spirit: nil}), do: nil
  def weapon(%Monster{spirit: %Spirit{equipment: equipment}}) do
    equipment
    |> Enum.find(fn(%{"worn_on" => worn_on}) ->
         worn_on in ["Weapon Hand", "Two Handed"]
       end)
  end

  def physical_damage(%Monster{} = monster) do
    str = strength(monster)
    agi = agility(monster)
    wil = will(monster)

    physical_damage(str, agi, wil)
  end

  def physical_damage(str, agi, _wil) do
    ((str * 2) + agi) / 20
  end

  def magical_damage(%Monster{} = monster) do
    str = strength(monster)
    agi = agility(monster)
    wil = will(monster)

    magical_damage(str, agi, wil)
  end

  def magical_damage(_str, agi, wil) do
    ((wil * 2) + agi) / 20
  end

  def strength(%Monster{} = monster) do
    attribute(monster, :strength)
  end

  def agility(%Monster{} = monster) do
    attribute(monster, :agility)
  end

  def will(%Monster{} = monster) do
    attribute(monster, :will)
  end

  def attribute(%Monster{spirit: nil, strength: nil, agility: nil, will: nil, level: level}, _attribute) do
    50 + (10 * level)
  end

  def attribute(%Monster{spirit: nil, strength: str}, :strength), do: str
  def attribute(%Monster{spirit: nil, agility:  agi}, :agility),  do: agi
  def attribute(%Monster{spirit: nil, will:    will}, :will),     do: will

  def attribute(%Monster{monster_template_id: nil} = monster, attribute) do
    attribute_from_equipment(monster, attribute)
  end

  def attribute(%Monster{} = monster, attribute) do
    monster_attribute =
      monster
      |> Map.put(:spirit, nil)
      |> attribute(attribute)

    useless_limbs = (monster.crippled_limbs ++ monster.missing_limbs) |> Enum.uniq |> length
    total_limbs = monster.limbs |> Map.keys |> length

    limb_modifier =
      if total_limbs > useless_limbs, do: 1 - (useless_limbs / total_limbs), else: 1

    trunc(max(1, attribute_from_equipment(monster, attribute) + monster_attribute + effect_bonus(monster, attribute)) * limb_modifier)
  end

  def attribute_from_equipment(%Monster{spirit: nil}, _), do: 0
  def attribute_from_equipment(%Monster{spirit: %Spirit{equipment: equipment}}, attribute) do
    Enum.reduce(equipment, 0, &(&2 + apply(ApathyDrive.Item, attribute, [&1])))
  end

  def hp_regen_per_second(%Monster{max_hp: max_hp, missing_limbs: []} = monster) do
    modifier = 1 + effect_bonus(monster, "hp_regen") / 100

    normal_regen = max_hp * 0.01 * modifier

    poison = effect_bonus(monster, "poison") / 10

    normal_regen - poison
  end
  def hp_regen_per_second(%Monster{max_hp: max_hp, missing_limbs: missing_limbs}) do
    -(length(missing_limbs) * 0.01 * max_hp)
  end

  def mana_regen_per_second(%Monster{max_mana: max_mana} = monster) do
    modifier = 1 + effect_bonus(monster, "mana_regen") / 100

    max_mana * 0.01 * modifier
  end

  def global_hated_targets(%Monster{hate: hate}) do
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

  def aggro_target(%Room{} = room, %Monster{} = monster) do
    targets = Room.local_hated_targets(room, monster)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def most_hated_target(%Monster{} = monster) do
    targets = global_hated_targets(monster)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def top_threat([]),      do: nil
  def top_threat(targets), do: Enum.max(targets)

  def look_name(monster, opts \\ [])
  def look_name(%Monster{} = monster, opts) do
    name =
      monster.name
      |> String.ljust(opts[:ljust] || 0)
      |> String.rjust(opts[:rjust] || 0)

    "<span class='#{alignment_color(monster)}'>#{name}</span>"
  end
  def look_name(%Character{} = character, opts) do
    name =
      character.name
      |> String.ljust(opts[:ljust] || 0)
      |> String.rjust(opts[:rjust] || 0)

    "<span class='dark-cyan'>#{name}</span>"
  end

  def track_data(%Monster{spirit: spirit} = monster) do
    %{
      monster: monster.ref,
      alignment: monster.alignment,
      unities: monster.unities,
      essence: monster.experience,
      spirit_unities: spirit && spirit.class.unities,
      spirit_essence: spirit && spirit.experience,
      spawned_at: monster.spawned_at,
      name: monster.name,
      keywords: String.split(monster.name),
      look_name: look_name(monster),
      invisible?: !monster.monster_template_id
    }
  end

  def save(%Monster{monster_template_id: nil, spirit: spirit} = monster) do
    Map.put(monster, :spirit, Repo.save!(Map.put(spirit, :experience, trunc(spirit.experience))))
  end

  def save(%Monster{monster_template_id: _, spirit: nil} = monster) do
    Repo.save!(Map.put(monster, :experience, trunc(monster.experience)))
  end

  def save(%Monster{monster_template_id: _, spirit: spirit} = monster) do
    monster
    |> Map.put(:experience, trunc(monster.experience))
    |> Map.put(:spirit, Repo.save!(Map.put(spirit, :experience, trunc(spirit.experience))))
    |> Repo.save!
  end

  def update_essence(%Monster{spirit: spirit} = monster, %Room{} = room) do
    current_essence = (spirit && spirit.experience) || monster.experience

    target_essence = target_essence(monster)

    difference = target_essence - current_essence
    amount_to_shift = difference / 5 / 60 * Room.essence_update_interval(room) / 1000

    percent_difference = if current_essence == 0, do: 1, else: abs(difference) / current_essence

    monster =
      cond do
        percent_difference == 0 or amount_to_shift == 0 ->
          monster
        percent_difference <= 0.10 ->
          add_experience(monster, difference)
        true ->
          add_experience(monster, amount_to_shift)
      end

    Character.update_prompt(monster)

    monster
  end

  defp target_essence(%Monster{spirit: spirit, room_essences: room_essences, unity_essences: unity_essences} = monster) do
    unities = (spirit && spirit.class.unities) || monster.unities

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
