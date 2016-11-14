defmodule ApathyDrive.Ability do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{PubSub, Monster, TimerManager, Ability, Match, Room, RoomServer}
  import ApathyDrive.Text
  import ApathyDrive.TimerManager, only: [seconds: 1]

  schema "abilities" do
    field :name, :string
    field :description, :string

    has_many :spells_abilities, ApathyDrive.SpellAbility
    has_many :spells, through: [:spells_abilities, :spell]

    has_many :items_abilities, ApathyDrive.ItemAbility
    has_many :items, through: [:items_abilities, :item]

    timestamps
  end

  @required_fields ~w(name description)
  @optional_fields ~w()


  #############
  # OLD SHIT BELOW
  ############

  def find(id) do
    ApathyDrive.Repo.get(__MODULE__, id)
    |> Map.get(:properties)
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(ability) ->
         "#{ability.properties["name"]} - #{ability.id}"
       end)
  end


  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def useable(abilities, %{mana: mana}) do
    abilities
    |> Enum.reject(fn(ability) ->
         ability["mana_cost"] && ability["mana_cost"] > mana
       end)
  end

  def removes_blessing?(%{} = monster, %{"instant_effects" => %{"remove abilities" => abilities}} = ability) do
    Systems.Effect.max_stacks?(monster, ability) or
    Enum.any?(abilities, fn(ability_id) ->
      Systems.Effect.stack_count(monster, ability_id) > 0
    end)
  end
  def removes_blessing?(monster, ability) do
    Systems.Effect.max_stacks?(monster, ability)
  end

  def color(%{"kind" => "attack"}),      do: "red"
  def color(%{"kind" => "room attack"}), do: "red"
  def color(%{"kind" => "curse"}),       do: "red"
  def color(%{"kind" => "room curse"}),  do: "red"
  def color(%{"kind" => _}),             do: "blue"

  def prep_message(nil, _, _, _, _), do: nil
  def prep_message(message, %{} = ability, %{} = user, %{} = target, interpolations) do
    message = message
              |> interpolate(Map.merge(%{"user" => user, "target" => target}, interpolations))
              |> capitalize_first
    "<p><span class='#{color(ability)}'>#{message}</span></p>"
  end

  def cast_messages(%{} = ability,
                    %{} = user,
                    %{} = target,
                    interpolations \\ %{},
                    message_key \\ "cast_message") do
    %{
      "user"      => prep_message(ability[message_key]["user"],      ability, user, target, interpolations),
      "target"    => prep_message(ability[message_key]["target"],    ability, user, target, interpolations),
      "spectator" => prep_message(ability[message_key]["spectator"], ability, user, target, interpolations)
    }
  end

  def scale_ability(%{} = monster, prop_name, %{"potency" => _} = ability) do
    scale_effect(monster, prop_name, ability)
  end
  def scale_ability(%{} = monster, prop_name, %{"base_min" => _, "base_max" => _} = ability) do
    scale_effect(monster, prop_name, ability)
  end
  def scale_ability(%{} = monster, _prop_name, %{} = ability) do
    ability
    |> Map.keys
    |> Enum.reduce(%{}, fn(key, map) ->
         Map.put(map, key, scale_ability(monster, key, ability[key]))
       end)
  end
  def scale_ability(%{}, _prop_name, ability) do
    ability
  end

  def scale_effect(%{} = _monster, value) when is_number(value), do: value
  def scale_effect(%{} = _monster, value) when is_binary(value), do: value

  def scale_effect(%{} = monster, "damage", %{"base_min" => base_min, "base_max" => base_max}) do
    base_max = base_max + Systems.Effect.effect_bonus(monster, "increase max damage")
    base_min..base_max
    |> Enum.random
  end

  def scale_effect(%{}, _effect_name, %{"base_min" => base_min, "base_max" => base_max}) do
    base_min..base_max
    |> Enum.random
  end

  def scale_effect(%{} = monster, "heal", %{"potency" => potency}) do
    average = (potency/300) * ((Monster.magical_damage(monster)) + (0.2229 * Monster.will(monster)))

    modifier = (80..120 |> Enum.random) / 100

    trunc(average * modifier)
  end

  def scale_effect(%{} = monster, "damage", %{"potency" => potency, "type" => type}) do
    if type == "magical" do
      trunc((potency/300) * (Monster.magical_damage(monster) + (0.2229 * Monster.will(monster))))
    else
      trunc((potency/300) * (Monster.physical_damage(monster) + (0.2229 * Monster.strength(monster))))
    end
  end

  def scale_effect(%{} = monster, "drain", %{"potency" => potency, "type" => type}) do
    if type == "magical" do
      trunc((potency/300) * (Monster.magical_damage(monster) + (0.2229 * Monster.will(monster))))
    else
      trunc((potency/300) * (Monster.physical_damage(monster) + (0.2229 * Monster.strength(monster))))
    end
  end

  def scale_effect(%{}, _effect_name, %{"potency" => potency}) do
    potency
  end

  def scale_effect(%{}, _effect_name, effect), do: effect

  def find_monster_in_room(%Room{} = room, %{}, query) do
    monsters =
      room.monsters
      |> Map.values

    target =
      monsters
      |> Match.one(:name_contains, query)

    target && target.ref
  end

  def find_other_monster_in_room(%Room{} = room, %{ref: monster_ref}, query) do
    monsters =
      room.monsters
      |> Map.values

    target =
      monsters
      |> Enum.reject(&(&1.ref == monster_ref))
      |> Match.one(:name_contains, query)

    target && target.ref
  end

  def alignment_enemies(%Room{} = room, %{} = monster) do
    room.monsters
    |> Map.values
    |> Enum.reject(&(&1.alignment == monster.alignment))
    |> Enum.map(&Map.get(&1, :ref))
  end

  def wrap_target(nil),    do: []
  def wrap_target(%{pid: pid}), do: [pid]
  def wrap_target(target), do: [target]

  def reject(%{pid: target_pid}, %{pid: pid}) when pid == target_pid, do: nil
  def reject(%{pid: target_pid}, %{}), do: target_pid
  def reject(target_pid, %{pid: pid}) when pid == target_pid, do: nil
  def reject(target_pid, %{}), do: target_pid

  def can_execute?(%Room{} = room, %{} = monster, ability) do
    cond do
      on_cooldown?(monster, ability) ->
        Monster.send_scroll(monster, "<p><span class='dark-cyan'>You can't do that yet.</p>")
        false
      Monster.confused(room, monster) ->
        false
      Monster.silenced(monster, ability) ->
        false
      not_enough_mana?(monster, ability) ->
        false
      true ->
        true
    end
  end

  def not_enough_mana?(%{mana: _mana}, %{"ignores_global_cooldown" => true}), do: false
  def not_enough_mana?(%{mana: mana} = monster, %{"mana_cost" => cost}) when cost > mana do
    Monster.send_scroll(monster, "<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
    true
  end
  def not_enough_mana?(%{}, %{}), do: false

  def cancel_cast_timer(%Room{} = room, monster_ref) do
    Room.update_monster(room, monster_ref, fn
      %{timers: %{cast_timer: _}} = monster ->
        Monster.send_scroll(monster, "<p><span class='dark-yellow'>You interrupt your spell.</span></p>")
        TimerManager.cancel(monster, :cast_timer)
      %{} = monster ->
        monster
    end)
  end

  def start_cast_timer(%Room{} = room, monster_ref, %{"cast_time" => time} = ability, target) do
    Room.update_monster(room, monster_ref, fn monster ->
      Monster.send_scroll(monster, "<p><span class='dark-yellow'>You begin your casting.</span></p>")
      TimerManager.send_after(monster, {:cast_timer, time |> seconds, {:timer_cast_ability, %{caster: monster.ref, ability: ability, timer: time, target: target}}})
    end)
  end

  def execute(%Room{} = room, monster_ref, %{"cast_time" => _time} = ability, target) do
    with %{} = monster <- room.monsters[monster_ref],
         %Room{} = room <- cancel_cast_timer(room, monster.ref) do
      start_cast_timer(room, monster_ref, ability, target)
    else
      _ ->
        room
    end
  end

  def execute(%Room{} = room, caster_ref, %{"kind" => kind}, "") when kind in ["attack", "curse"] do
    room
    |> Room.get_monster(caster_ref)
    |> Monster.send_scroll("<p>You must specify a target.</p>")

    room
  end

  def execute(%Room{} = room, caster_ref, %{} = ability, query) when is_binary(query) do
    monster =
      room
      |> Room.get_monster(caster_ref)

    room
    |> can_execute?(monster, ability)
    |> if do
         targets = get_targets(room, Room.get_monster(room, caster_ref), ability, query)
         execute(room, caster_ref, ability, targets)
       else
         room
       end
  end

  def execute(%Room{} = room, caster_ref, %{} = ability, targets) do
    monster = Room.get_monster(room, caster_ref)

    if monster do
      #display_pre_cast_message(room, caster_ref, ability)

      room =
        Room.update_monster(room, caster_ref, fn(monster) ->
          monster =
            monster
            |> apply_cooldown(ability)
            |> Map.put(:mana, monster.mana - Map.get(ability, "mana_cost", 0))

            Mobile.update_prompt(monster)
          monster
        end)

      ability =
        room
        |> Room.get_monster(caster_ref)
        |> scale_ability(nil, ability)

      targets
      |> Enum.reduce(room, fn(target_ref, updated_room) ->
           cond do
             updated_room |> Room.get_monster(target_ref) |> affects_target?(ability) ->
               updated_room
               |> apply_ability(target_ref, ability, caster_ref)
               |> kill_monsters(caster_ref)
             target = Room.get_monster(updated_room, target_ref) ->
               message = "#{target.name} is not affected by that ability." |> capitalize_first
               Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{message}</span></p>")
               updated_room
             true ->
               updated_room
           end
         end)
      |> execute_multi_cast(caster_ref, ability, targets)
    else
      room
    end
  end

  def kill_monsters(%Room{} = room, caster_ref) do
    Enum.reduce(room.monsters, room, fn {_ref, target}, updated_room ->
      if target.hp < 1 or (target.spirit && target.spirit.experience < -99) do
        #ApathyDrive.Death.kill(updated_room, target.ref, caster_ref)
      else
        Mobile.update_prompt(target)
        updated_room
      end
    end)
  end

  def execute_multi_cast(%Room{} = room, caster_ref, %{"multi-cast" => times} = ability, targets) do
    ability = Map.delete(ability, "multi-cast")

    room =
      Enum.reduce(2..times, room, fn(_, updated_room) ->
        ability =
          ability
          |> Map.delete("pre-cast_message")
          |> Map.delete("global_cooldown")
          |> Map.put("ignores_global_cooldown", true)

        execute(updated_room, caster_ref, ability, targets)
      end)

    execute(room, caster_ref, ability, targets)
  end
  def execute_multi_cast(%Room{} = room, _caster_ref, %{} = _ability, _targets), do: room

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "attack"}, "") do
    room
    |> Monster.aggro_target(monster)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "attack"}, query) do
    find_other_monster_in_room(room, monster, query)
    |> wrap_target
  end

  def get_targets(%Room{}, %{ref: ref}, %{"kind" => "blessing"}, "") do
    wrap_target(ref)
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind": "blessing"}, query) do
    find_monster_in_room(room, monster, query)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "curse"}, "") do
    room
    |> Monster.aggro_target(monster)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "curse"}, query) do
    room
    |> find_other_monster_in_room(monster, query)
    |> wrap_target
  end

  def get_targets(%Room{}, %{} = monster, %{"kind" => "heal"}, "") do
    monster.ref
    |> wrap_target
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "heal"}, query) do
    room
    |> find_monster_in_room(monster, query)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "room attack"}, _query) do
    Enum.uniq(alignment_enemies(room, monster) ++ Room.local_hated_targets(room, monster))
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "room blessing"}, _query) do
    room.mobles
    |> Map.values
    |> Enum.filter(&(&1.alignment == monster.alignment))
    |> Enum.map(&Map.get(&1, :ref))
    |> Kernel.--(Room.local_hated_targets(room, monster))
    |> Enum.uniq
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "room curse"}, _query) do
    Enum.uniq(alignment_enemies(room, monster) ++ Room.local_hated_targets(room, monster))
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "room heal"}, _query) do
    room.mobles
    |> Map.values
    |> Enum.filter(&(&1.alignment == monster.alignment))
    |> Enum.map(&Map.get(&1, :ref))
    |> Kernel.--(Room.local_hated_targets(room, monster))
    |> Enum.uniq
  end

  def get_targets(%Room{}, %{} = monster, %{"kind" => "utility"}, "") do
    wrap_target(monster.ref)
  end

  def get_targets(%Room{} = room, %{} = monster, %{"kind" => "utility"}, query) do
    room
    |> find_monster_in_room(monster, query)
    |> wrap_target
  end

  def attack_abilities(%{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1["kind"] == "attack"))
    |> useable(monster)
  end

  def bless_abilities(%{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1["kind"] == "blessing"))
    |> Enum.reject(fn(ability) ->
         Ability.removes_blessing?(monster, ability)
       end)
    |> useable(monster)
  end

  def curse_abilities(%{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1["kind"] == "curse"))
    |> Enum.reject(fn(ability) ->
         Ability.removes_blessing?(monster, ability)
       end)
    |> useable(monster)
  end

  def heal_abilities(%{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1["kind"] == "heal"))
    |> useable(monster)
  end

  def on_cooldown?(%{effects: effects} = monster, %{"cooldown" => _, "name" => name} = ability) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == name)) or on_global_cooldown?(monster, ability)
  end
  def on_cooldown?(%{} = monster, %{} = ability) do
    on_global_cooldown?(monster, ability)
  end

  def on_global_cooldown?(%{},          %{"ignores_global_cooldown" => true}), do: false
  def on_global_cooldown?(%{} = monster, %{}), do: on_global_cooldown?(monster)
  def on_global_cooldown?(%{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == :global))
  end

  def apply_cooldown(%{} = monster, %{"name" => "attack"} = ability) do
    if gc = global_cooldown(ability, monster) do
      Systems.Effect.add(monster, %{"cooldown" => :attack}, gc)
    else
      monster
    end
  end

  def apply_cooldown(%{} = monster, %{"cooldown" => cooldown, "name" => name} = ability) do
    monster
    |> Systems.Effect.add(%{"cooldown" => name,
                            "expiration_message" => "#{capitalize_first(name)} is ready for use again."},
                          cooldown)
    |> apply_cooldown(Map.delete(ability, "cooldown"))
  end

  def apply_cooldown(%{} = monster, %{} = ability) do
    if gc = global_cooldown(ability, monster) do
      Systems.Effect.add(monster, %{"cooldown" => :global}, gc)
    else
      monster
    end
  end

  def after_cast(%{"ability_user" => caster_ref,
                   "ability" => after_cast_ability,
                   "chance" => chance}, targets) do
    if chance >= :rand.uniform(100) do
      execute_after_cast(caster_ref, after_cast_ability, targets)
    end
  end
  def after_cast(%{"ability_user" => caster_ref,
                   "ability"   => after_cast_ability}, targets) do
    execute_after_cast(caster_ref, after_cast_ability, targets)
  end
  def after_cast(%{}, _targets), do: false

  def execute_after_cast(caster_ref, after_cast_ability, targets) do
    ability =
      after_cast_ability
      |> find()
      |> Map.put("ignores_global_cooldown", true)

    send(self, {:execute_ability, %{caster: caster_ref, ability: ability, target: targets}})
  end

  def global_cooldown(%{"ignores_global_cooldown" => true}, %{}), do: nil
  def global_cooldown(%{"global_cooldown" => gc}, %{effects: effects}) do
    speed_mods = effects
                 |> Map.values
                 |> Enum.filter(&(Map.has_key?(&1, "speed")))
                 |> Enum.map(&(Map.get(&1, "speed")))

    gc * speed_modifier(speed_mods)
  end
  def global_cooldown(%{}, %{}), do: nil

  def speed_modifier([]), do: 1
  def speed_modifier(speed_mods) do
    count = length(speed_mods)

    Enum.sum(speed_mods) / count / 100
  end

  def dodged?(%{} = monster, %{"accuracy_stats" => stats}, %{} = attacker) do
    accuracy =
      stats
      |> Enum.reduce(0, fn(stat, total) ->
           total + Monster.attribute(attacker, String.to_existing_atom(stat))
         end)
      |> div(length(stats))
      |> Kernel.+(Systems.Effect.effect_bonus(monster, "Accuracy"))

    dodge = Monster.agility(monster) + Systems.Effect.effect_bonus(monster, "Dodge")

    if dodge > 0 do
      chance = dodge_chance(dodge, accuracy)

      :rand.uniform(100) < trunc(chance)
    else
      false
    end
  end

  def dodge_chance(dodge, accuracy)
  def dodge_chance(_dodge, accuracy) when accuracy < 1, do: 100
  def dodge_chance(dodge, _accuracy) when dodge < 1, do: 0
  def dodge_chance(dodge, accuracy) when dodge == accuracy, do: 10
  def dodge_chance(dodge, accuracy) do
    ratio = dodge / accuracy

    if ratio < 1 do
      max(trunc(10 - ((1.0 - ratio) / 0.1) * 2), 1)
    else
      max(min(trunc(((3.0 + ratio) * 87) - 340), 90), 10)
    end
  end

  def apply_ability(%Room{} = room, target_ref, %{"dodge_message" => _, "accuracy_stats" => _} = ability, caster_ref) do
    monster = Room.get_monster(room, target_ref)
    ability_user = Room.get_monster(room, caster_ref)

    if dodged?(monster, ability, ability_user) do

      Enum.each(room.monsters, fn {ref, room_monster} ->
        cond do
          ref == ability_user.ref ->
            user_message = interpolate(ability["dodge_message"]["user"], %{"user" => ability_user, "target" => monster})
            Monster.send_scroll(room_monster, "<p><span class='dark-cyan'>#{user_message}</span></p>")
          ref == monster.ref ->
            target_message = interpolate(ability["dodge_message"]["target"], %{"user" => ability_user, "target" => monster})
            Monster.send_scroll(room_monster, "<p><span class='dark-cyan'>#{target_message}</span></p>")
          :else ->
            spectator_message = interpolate(ability["dodge_message"]["spectator"], %{"user" => ability_user, "target" => monster})
            Monster.send_scroll(room_monster, "<p><span class='dark-cyan'>#{spectator_message}</span></p>")
        end
      end)

      2000
      |> :rand.uniform
      |> :erlang.send_after(self, {:think, target_ref})

      Room.update_monster(room, target_ref, fn target ->
        put_in(target.hate, Map.update(target.hate, caster_ref, 1, fn(hate) -> hate + 1 end))
      end)
    else
      ability =
        ability
        |> Map.drop(["dodge_message", "accuracy_stats"])

      apply_ability(room, target_ref, ability, caster_ref)
    end
  end
  def apply_ability(%Room{} = room, target_ref, %{} = ability, caster_ref) do

    room = if Enum.member?(["curse", "room curse"], ability["kind"]) do
      Room.update_monster(room, target_ref, fn target ->
        put_in(target.hate, Map.update(target.hate, caster_ref, 1, fn(hate) -> hate + 1 end))
      end)
    else
      room
    end

    ability = reduce_damage(room, ability, target_ref, caster_ref)

    2000
    |> :rand.uniform
    |> :erlang.send_after(self, {:think, target_ref})

    display_cast_message(room, target_ref, ability, caster_ref)

    room
    |> apply_instant_effects(target_ref, ability["instant_effects"], caster_ref)
    |> add_duration_effects(target_ref, ability, caster_ref)
  end

  def reduce_damage(%Room{} = room, %{"instant_effects" => %{"damage" => damage}} = ability, target_ref, caster_ref) do
    target = Room.get_monster(room, target_ref)
    caster = Room.get_monster(room, caster_ref)

    min_damage = trunc(80 * damage / 100)  + Systems.Effect.effect_bonus(caster, "increase min damage")
    max_damage = trunc(120 * damage / 100) + Systems.Effect.effect_bonus(caster, "increase max damage")
    damage = min_damage..max_damage |> Enum.random

    damage = Monster.reduce_damage(target, damage, ability["instant_effects"]["mitigated_by"])

    put_in(ability, ["instant_effects", "damage"], max(damage, 1))
  end
  def reduce_damage(%Room{} = room, %{"instant_effects" => %{"drain" => drain}} = ability, target_ref, caster_ref) do
    reduce_damage(room, put_in(ability, ["instant_effects", "damage"], drain), target_ref, caster_ref)
  end
  def reduce_damage(%Room{}, %{} = ability, _target_ref, _caster_ref) do
    ability
  end

  def trigger_damage_shields(%Room{} = room, target_ref, caster_ref) when target_ref == caster_ref, do: room
  def trigger_damage_shields(%Room{} = room, target_ref, caster_ref) do
    if target = Room.get_monster(room, target_ref) do
      target
      |> Map.get(:effects)
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "damage shield")))
      |> Enum.reduce(room, fn
           %{"damage shield" => damage, "damage shield message" => message, "damage shield type" => damage_type}, updated_room ->
             ability = %{"kind" => "attack", "flags" => [], "instant_effects" => %{"damage" => damage}, "damage_type" => damage_type, "cast_message" => message}
             execute(updated_room, target_ref, ability, [caster_ref])
         end)
    else
      room
    end
  end

  def apply_instant_effects(%Room{} = room, _target_ref, nil, _caster_ref), do: room
  def apply_instant_effects(%Room{} = room, _target_ref, %{} = effects, _caster_ref) when map_size(effects) == 0, do: room
  def apply_instant_effects(%Room{} = room, target_ref, %{"drain" => _} = effects, caster_ref) do
    room
    |> execute(caster_ref, %{"instant_effects" => %{"heal" => effects["damage"]}}, [caster_ref])
    |> apply_instant_effects(target_ref, Map.delete(effects, "drain"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"damage" => damage} = effects, caster_ref) do
    room =
      Room.update_monster(room, target_ref, fn target ->
        target = Map.put(target, :hp, target.hp - damage)

        if target_ref == caster_ref do
          target
        else
          enmity = trunc(damage * Map.get(effects, "hate_multiplier", 1.0))
          put_in(target.hate, Map.update(target.hate, caster_ref, damage, fn(hate) -> hate + enmity end))
        end
      end)

    room =
      if target = Room.get_monster(room, target_ref) do
        chance = trunc((damage / max(1, target.hp)) * 100)

        apply_criticals(room, caster_ref, target_ref, chance, effects["crit_tables"])
      else
        room
      end

    room = trigger_damage_shields(room, target_ref, caster_ref)

    effects =
      effects
      |> Map.delete("damage")
      |> Map.delete("hate_multiplier")
      |> Map.delete("crit_tables")

    apply_instant_effects(room, target_ref, effects, caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"heal" => heal} = effects, caster_ref) do
    room
    |> Room.update_monster(target_ref, fn monster ->
         put_in(monster.hp, min(monster.max_hp, monster.hp + heal))
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "heal"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"heal_mana" => heal} = effects, caster_ref) do
    room
    |> Room.update_monster(target_ref, fn monster ->
         put_in(monster.mana, min(monster.max_mana, monster.mana + heal))
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "heal_mana"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"script" => script} = effects, caster_ref) do
    script = ApathyDrive.Script.find(script)

    room
    |> Room.update_monster(target_ref, fn monster ->
         ApathyDrive.Script.execute(room, monster, script)
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "script"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"remove abilities" => abilities} = effects, caster_ref) do
    room
    |> Room.update_monster(target_ref, fn monster ->
         Enum.reduce(abilities, monster, fn(ability_id, updated_monster) ->
           Systems.Effect.remove_oldest_stack(updated_monster, ability_id)
         end)
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "remove abilities"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"teleport" => room_ids} = effects, caster_ref) do
    destination = Enum.random(room_ids)

    room_exit =
      %{
        "kind" => "Action",
        "destination" => destination,
        "mover_message" => "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
        "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
        "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
      }

    monster = Room.get_monster(room, target_ref)

    room = ApathyDrive.Commands.Move.execute(room, monster, room_exit)

    apply_instant_effects(room, target_ref, Map.delete(effects, "teleport"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"dispel" => effect_types} = effects, caster_ref) do
    room
    |> Room.update_monster(target_ref, fn monster ->
         Enum.reduce(effect_types, monster, fn(type, updated_monster) ->
           effects_with_type =
             monster.effects
             |> Map.keys
             |> Enum.filter(fn(key) ->
                  effect = monster.effects[key]
                  if key == "all" do
                    # match all temporary effects (effects with timers) for "all"
                     Map.has_key?(effect, "timers")
                  else
                    # match all temporary effects with the matching key (e.g. poison, blinded)
                     Map.has_key?(effect, type) and Map.has_key?(effect, "timers")
                  end
                end)

           Enum.reduce(effects_with_type, updated_monster, fn(ability_id, sub_updated_monster) ->
             Systems.Effect.remove(sub_updated_monster, ability_id, show_expiration_message: true)
           end)
         end)
       end)

    apply_instant_effects(room, target_ref, Map.delete(effects, "dispel"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"kill" => true} = effects, caster_ref) do
    room =
      Room.update_monster(room, target_ref, fn monster ->
        Map.put(monster, :hp, 0)
      end)

    apply_instant_effects(room, target_ref, Map.delete(effects, "kill"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"limb_loss" => limb_loss} = effects, caster_ref) do

    room =
      Room.update_monster(room, target_ref, fn monster ->
        Enum.reduce(limb_loss, monster, fn
          %{"kind" => "cripple", "limb" => limb}, updated_monster ->
            case Monster.uncrippled_limb(updated_monster, limb) do
              nil ->
                updated_monster
              limb ->
                Monster.send_scroll(updated_monster, "<p>Your #{limb} is crippled!</p>")
                Room.send_scroll(room, "<p>#{Monster.look_name(updated_monster)}'s #{limb} is crippled!</p>", [updated_monster])
                update_in(updated_monster.crippled_limbs, &([limb | &1]))
            end
          %{"kind" => "sever", "limb" => limb}, updated_monster ->
            case Monster.unsevered_limb(updated_monster, limb) do
              nil ->
                updated_monster
              limb ->
                Monster.send_scroll(updated_monster, "<p>Your #{limb} has been severed!</p>")
                Room.send_scroll(room, "<p>#{Monster.look_name(updated_monster)}'s #{limb} has been severed!</p>", [updated_monster])
                update_in(updated_monster.missing_limbs, &([limb | &1]))
            end
        end)

      end)

    apply_instant_effects(room, target_ref, Map.delete(effects, "limb_loss"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{} = effects, caster_ref) do
    room
    |> Room.get_monster(caster_ref)
    |> Monster.send_scroll("<p><span class='red'>unrecognized instant effects: #{inspect Map.keys(effects)}</span></p>")

    apply_instant_effects(room, target_ref, %{}, caster_ref)
  end

  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, damage, _crit_tables) when damage < 1, do: room
  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, _damage, nil), do: room
  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, _damage, []), do: room
  def apply_criticals(%Room{} = room, caster_ref, target_ref, damage, crit_tables) do
    damage
    |> ApathyDrive.Crit.abilities(crit_tables)
    |> Enum.reduce(room, fn
         %{"kind" => kind} = ability, updated_room when kind in ["attack", "curse"] ->
           execute(updated_room, caster_ref, ability, [target_ref])
         %{} = ability, updated_room ->
           execute(updated_room, caster_ref, ability, "")
       end)
  end

  def display_cast_message(room, target_ref, %{"cast_message" => _, "instant_effects" => %{"heal" => heal}} = ability, caster_ref) do
    target = Room.get_monster(room, target_ref)
    caster = Room.get_monster(room, caster_ref)

    cast_messages = cast_messages(ability, caster, target, %{"amount" => heal})

    room.monsters
    |> Enum.each(fn {ref, monster} ->
         cond do
           ref == caster_ref ->
             Monster.send_scroll(monster, cast_messages["user"])
           ref == target_ref ->
             Monster.send_scroll(monster, cast_messages["target"])
           true ->
             Monster.send_scroll(monster, cast_messages["spectator"])
         end
       end)
  end

  def display_cast_message(room, target_ref, %{"cast_message" => _, "instant_effects" => %{"damage" => damage}} = ability, caster_ref) do
    target = Room.get_monster(room, target_ref)
    caster = Room.get_monster(room, caster_ref)

    cast_messages = cast_messages(ability, caster, target, %{"amount" => damage})

    room.monsters
    |> Enum.each(fn {ref, monster} ->
         cond do
           ref == caster_ref ->
             Monster.send_scroll(monster, cast_messages["user"])
           ref == target_ref ->
             Monster.send_scroll(monster, cast_messages["target"])
           true ->
             Monster.send_scroll(monster, cast_messages["spectator"])
         end
       end)
  end

  def display_cast_message(room, target_ref, %{"cast_message" => _} = ability, caster_ref) do
    target = Room.get_monster(room, target_ref)
    caster = Room.get_monster(room, caster_ref)

    cast_messages = cast_messages(ability, caster, target)

    room.monsters
    |> Enum.each(fn {ref, monster} ->
         cond do
           ref == caster_ref ->
             Monster.send_scroll(monster, cast_messages["user"])
           ref == target_ref ->
             Monster.send_scroll(monster, cast_messages["target"])
           true ->
             Monster.send_scroll(monster, cast_messages["spectator"])
         end
       end)
  end

  def display_cast_message(_room, _target_ref, %{} = _ability, _caster_ref), do: :noop

  def add_duration_effects(%Room{} = room,
                           target_ref,
                           %{
                               "duration_effects" => %{
                                 "stack_key"   => _stack_key,
                                 "stack_count" => _stack_count
                               } = effects
                             } = ability,
                            caster_ref) do

     effects =
       if Map.has_key? effects, "after_cast" do
         put_in effects, ["after_cast", "ability_user"], caster_ref
       else
         effects
       end

    Room.update_monster(room, target_ref, fn monster ->
      monster
      |> Systems.Effect.add(effects, Map.get(ability, "duration", 0))
      |> Monster.send_scroll("<p><span class='#{Ability.color(ability)}'>#{effects["effect_message"]}</span></p>")
    end)
  end
  def add_duration_effects(%Room{} = room,
                           target_ref,
                           %{
                               "duration_effects" => effects
                           } = ability,
                          caster_ref) do

    effects = effects
              |> Map.put("stack_key",   ability["name"])
              |> Map.put("stack_count", 1)

    ability = Map.put(ability, "duration_effects", effects)

    add_duration_effects(room, target_ref, ability, caster_ref)
  end
  def add_duration_effects(%Room{} = room, _target_ref, %{}, _caster_ref), do: room

  def affects_target?(%{flags: _monster_flags}, %{"flags" => nil}), do: true
  def affects_target?(%{flags: monster_flags}, %{"flags" => ability_flags}) do
    cond do
      Enum.member?(ability_flags, "affects-living") and Enum.member?(monster_flags, "non-living") ->
        false
      Enum.member?(ability_flags, "affects-animals") and !Enum.member?(monster_flags, "animal") ->
        false
      Enum.member?(ability_flags, "affects-undead") and !Enum.member?(monster_flags, "undead") ->
        false
      Enum.member?(ability_flags, "poison") and Enum.member?(monster_flags, "poison-immunity") ->
        false
      true ->
        true
    end
  end
  def affects_target?(%{flags: _monster_flags}, %{}), do: true
  def affects_target?(nil, %{}), do: false

end
