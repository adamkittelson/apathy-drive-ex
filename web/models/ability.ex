defmodule ApathyDrive.Ability do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{PubSub, Mobile, TimerManager, Ability, Match, Room, RoomServer}
  import ApathyDrive.Text
  import ApathyDrive.TimerManager, only: [seconds: 1]

  schema "abilities" do
    field :properties, ApathyDrive.JSONB

    has_many :rooms, ApathyDrive.Room

    timestamps
  end

  @required_fields ~w(properties)
  @optional_fields ~w()

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

  def useable(abilities, %Mobile{mana: mana}) do
    abilities
    |> Enum.reject(fn(ability) ->
         ability["mana_cost"] && ability["mana_cost"] > mana
       end)
  end

  def removes_blessing?(%Mobile{} = mobile, %{"instant_effects" => %{"remove abilities" => abilities}} = ability) do
    Systems.Effect.max_stacks?(mobile, ability) or
    Enum.any?(abilities, fn(ability_id) ->
      Systems.Effect.stack_count(mobile, ability_id) > 0
    end)
  end
  def removes_blessing?(mobile, ability) do
    Systems.Effect.max_stacks?(mobile, ability)
  end

  def color(%{"kind" => "attack"}),      do: "red"
  def color(%{"kind" => "room attack"}), do: "red"
  def color(%{"kind" => "curse"}),       do: "red"
  def color(%{"kind" => "room curse"}),  do: "red"
  def color(%{"kind" => _}),             do: "blue"

  def prep_message(nil, _, _, _, _), do: nil
  def prep_message(message, %{} = ability, %Mobile{} = user, %Mobile{} = target, interpolations) do
    message = message
              |> interpolate(Map.merge(%{"user" => user, "target" => target}, interpolations))
              |> capitalize_first
    "<p><span class='#{color(ability)}'>#{message}</span></p>"
  end

  def cast_messages(%{} = ability,
                    %Mobile{} = user,
                    %Mobile{} = target,
                    interpolations \\ %{},
                    message_key \\ "cast_message") do
    %{
      "user"      => prep_message(ability[message_key]["user"],      ability, user, target, interpolations),
      "target"    => prep_message(ability[message_key]["target"],    ability, user, target, interpolations),
      "spectator" => prep_message(ability[message_key]["spectator"], ability, user, target, interpolations)
    }
  end

  def scale_ability(%Mobile{} = mobile, prop_name, %{"potency" => _} = ability) do
    scale_effect(mobile, prop_name, ability)
  end
  def scale_ability(%Mobile{} = mobile, prop_name, %{"base_min" => _, "base_max" => _} = ability) do
    scale_effect(mobile, prop_name, ability)
  end
  def scale_ability(%Mobile{} = mobile, _prop_name, %{} = ability) do
    ability
    |> Map.keys
    |> Enum.reduce(%{}, fn(key, map) ->
         Map.put(map, key, scale_ability(mobile, key, ability[key]))
       end)
  end
  def scale_ability(%Mobile{}, _prop_name, ability) do
    ability
  end

  def scale_effect(%Mobile{} = _mobile, value) when is_number(value), do: value
  def scale_effect(%Mobile{} = _mobile, value) when is_binary(value), do: value

  def scale_effect(%Mobile{} = mobile, "damage", %{"base_min" => base_min, "base_max" => base_max}) do
    base_max = base_max + Mobile.effect_bonus(mobile, "increase max damage")
    base_min..base_max
    |> Enum.random
  end

  def scale_effect(%Mobile{}, _effect_name, %{"base_min" => base_min, "base_max" => base_max}) do
    base_min..base_max
    |> Enum.random
  end

  def scale_effect(%Mobile{} = mobile, "heal", %{"potency" => potency}) do
    average = (potency/300) * ((Mobile.magical_damage(mobile)) + (0.2229 * Mobile.will(mobile)))

    modifier = (80..120 |> Enum.random) / 100

    trunc(average * modifier)
  end

  def scale_effect(%Mobile{} = mobile, "damage", %{"potency" => potency, "type" => type}) do
    if type == "magical" do
      trunc((potency/300) * (Mobile.magical_damage(mobile) + (0.2229 * Mobile.will(mobile))))
    else
      trunc((potency/300) * (Mobile.physical_damage(mobile) + (0.2229 * Mobile.strength(mobile))))
    end
  end

  def scale_effect(%Mobile{} = mobile, "drain", %{"potency" => potency, "type" => type}) do
    if type == "magical" do
      trunc((potency/300) * (Mobile.magical_damage(mobile) + (0.2229 * Mobile.will(mobile))))
    else
      trunc((potency/300) * (Mobile.physical_damage(mobile) + (0.2229 * Mobile.strength(mobile))))
    end
  end

  def scale_effect(%Mobile{}, _effect_name, %{"potency" => potency}) do
    potency
  end

  def scale_effect(%Mobile{}, _effect_name, effect), do: effect

  def find_mobile_in_room(%Room{} = room, %Mobile{}, query) do
    mobiles =
      room.mobiles
      |> Map.values

    target =
      mobiles
      |> Match.one(:name_contains, query)

    target && target.ref
  end

  def find_other_mobile_in_room(%Room{} = room, %Mobile{ref: mobile_ref}, query) do
    mobiles =
      room.mobiles
      |> Map.values

    target =
      mobiles
      |> Enum.reject(&(&1.ref == mobile_ref))
      |> Match.one(:name_contains, query)

    target && target.ref
  end

  def alignment_enemies(%Room{} = room, %Mobile{} = mobile) do
    room.mobiles
    |> Map.values
    |> Enum.reject(&(&1.alignment == mobile.alignment))
    |> Enum.map(&Map.get(&1, :ref))
  end

  def wrap_target(nil),    do: []
  def wrap_target(%Mobile{pid: pid}), do: [pid]
  def wrap_target(target), do: [target]

  def reject(%Mobile{pid: target_pid}, %Mobile{pid: pid}) when pid == target_pid, do: nil
  def reject(%Mobile{pid: target_pid}, %Mobile{}), do: target_pid
  def reject(target_pid, %Mobile{pid: pid}) when pid == target_pid, do: nil
  def reject(target_pid, %Mobile{}), do: target_pid

  def display_pre_cast_message(%Room{} = room, caster_ref, %{"pre-cast_message" => _} = ability) do
    mobile = Room.get_mobile(room, caster_ref)

    cast_messages = cast_messages(ability, mobile, mobile, %{}, "pre-cast_message")

    Mobile.send_scroll(mobile, cast_messages["user"])

    Room.send_scroll(room, cast_messages["spectator"], mobile)
  end
  def display_pre_cast_message(%Room{}, _caster_ref, %{}), do: nil

  def can_execute?(%Room{} = room, %Mobile{} = mobile, ability) do
    cond do
      on_cooldown?(mobile, ability) ->
        Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>You can't do that yet.</p>")
        false
      Mobile.confused(room, mobile) ->
        false
      Mobile.silenced(mobile, ability) ->
        false
      not_enough_mana?(mobile, ability) ->
        false
      true ->
        true
    end
  end

  def not_enough_mana?(%Mobile{mana: _mana}, %{"ignores_global_cooldown" => true}), do: false
  def not_enough_mana?(%Mobile{mana: mana} = mobile, %{"mana_cost" => cost}) when cost > mana do
    Mobile.send_scroll(mobile, "<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
    true
  end
  def not_enough_mana?(%Mobile{}, %{}), do: false

  def cancel_cast_timer(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn
      %Mobile{timers: %{cast_timer: _}} = mobile ->
        Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You interrupt your spell.</span></p>")
        TimerManager.cancel(mobile, :cast_timer)
      %Mobile{} = mobile ->
        mobile
    end)
  end

  def start_cast_timer(%Room{} = room, mobile_ref, %{"cast_time" => time} = ability, target) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You begin your casting.</span></p>")
      TimerManager.send_after(mobile, {:cast_timer, time |> seconds, {:timer_cast_ability, %{caster: mobile.ref, ability: ability, timer: time, target: target}}})
    end)
  end

  def execute(%Room{} = room, mobile_ref, %{"cast_time" => _time} = ability, target) do
    with %Mobile{} = mobile <- room.mobiles[mobile_ref],
         %Room{} = room <- cancel_cast_timer(room, mobile.ref) do
      start_cast_timer(room, mobile_ref, ability, target)
    else
      _ ->
        room
    end
  end

  def execute(%Room{} = room, caster_ref, %{"kind" => kind}, "") when kind in ["attack", "curse"] do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.send_scroll("<p>You must specify a target.</p>")

    room
  end

  def execute(%Room{} = room, caster_ref, %{} = ability, query) when is_binary(query) do
    mobile =
      room
      |> Room.get_mobile(caster_ref)

    room
    |> can_execute?(mobile, ability)
    |> if do
         targets = get_targets(room, Room.get_mobile(room, caster_ref), ability, query)
         execute(room, caster_ref, ability, targets)
       else
         room
       end
  end

  def execute(%Room{} = room, caster_ref, %{} = ability, targets) do
    mobile = Room.get_mobile(room, caster_ref)

    if mobile do
      display_pre_cast_message(room, caster_ref, ability)

      room =
        Room.update_mobile(room, caster_ref, fn(mobile) ->
          mobile =
            mobile
            |> apply_cooldown(ability)
            |> Map.put(:mana, mobile.mana - Map.get(ability, "mana_cost", 0))

            Mobile.update_prompt(mobile)
          mobile
        end)

      ability =
        room
        |> Room.get_mobile(caster_ref)
        |> scale_ability(nil, ability)

      targets
      |> Enum.reduce(room, fn(target_ref, updated_room) ->
           cond do
             updated_room |> Room.get_mobile(target_ref) |> affects_target?(ability) ->
               updated_room
               |> apply_ability(target_ref, ability, caster_ref)
               |> kill_mobiles(caster_ref)
             target = Room.get_mobile(updated_room, target_ref) ->
               message = "#{target.name} is not affected by that ability." |> capitalize_first
               Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{message}</span></p>")
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

  def kill_mobiles(%Room{} = room, caster_ref) do
    Enum.reduce(room.mobiles, room, fn {_ref, target}, updated_room ->
      if target.hp < 1 or (target.spirit && target.spirit.experience < -99) do
        ApathyDrive.Death.kill(updated_room, target.ref, caster_ref)
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

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "attack"}, "") do
    room
    |> Mobile.aggro_target(mobile)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "attack"}, query) do
    find_other_mobile_in_room(room, mobile, query)
    |> wrap_target
  end

  def get_targets(%Room{}, %Mobile{ref: ref}, %{"kind" => "blessing"}, "") do
    wrap_target(ref)
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind": "blessing"}, query) do
    find_mobile_in_room(room, mobile, query)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "curse"}, "") do
    room
    |> Mobile.aggro_target(mobile)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "curse"}, query) do
    room
    |> find_other_mobile_in_room(mobile, query)
    |> wrap_target
  end

  def get_targets(%Room{}, %Mobile{} = mobile, %{"kind" => "heal"}, "") do
    mobile.ref
    |> wrap_target
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "heal"}, query) do
    room
    |> find_mobile_in_room(mobile, query)
    |> wrap_target
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "room attack"}, _query) do
    Enum.uniq(alignment_enemies(room, mobile) ++ Room.local_hated_targets(room, mobile))
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "room blessing"}, _query) do
    room.mobles
    |> Map.values
    |> Enum.filter(&(&1.alignment == mobile.alignment))
    |> Enum.map(&Map.get(&1, :ref))
    |> Kernel.--(Room.local_hated_targets(room, mobile))
    |> Enum.uniq
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "room curse"}, _query) do
    Enum.uniq(alignment_enemies(room, mobile) ++ Room.local_hated_targets(room, mobile))
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "room heal"}, _query) do
    room.mobles
    |> Map.values
    |> Enum.filter(&(&1.alignment == mobile.alignment))
    |> Enum.map(&Map.get(&1, :ref))
    |> Kernel.--(Room.local_hated_targets(room, mobile))
    |> Enum.uniq
  end

  def get_targets(%Room{}, %Mobile{} = mobile, %{"kind" => "utility"}, "") do
    wrap_target(mobile.ref)
  end

  def get_targets(%Room{} = room, %Mobile{} = mobile, %{"kind" => "utility"}, query) do
    room
    |> find_mobile_in_room(mobile, query)
    |> wrap_target
  end

  def attack_abilities(%Mobile{abilities: abilities} = mobile) do
    abilities
    |> Enum.filter(&(&1["kind"] == "attack"))
    |> useable(mobile)
  end

  def bless_abilities(%Mobile{abilities: abilities} = mobile) do
    abilities
    |> Enum.filter(&(&1["kind"] == "blessing"))
    |> Enum.reject(fn(ability) ->
         Ability.removes_blessing?(mobile, ability)
       end)
    |> useable(mobile)
  end

  def curse_abilities(%Mobile{abilities: abilities} = mobile) do
    abilities
    |> Enum.filter(&(&1["kind"] == "curse"))
    |> Enum.reject(fn(ability) ->
         Ability.removes_blessing?(mobile, ability)
       end)
    |> useable(mobile)
  end

  def heal_abilities(%Mobile{abilities: abilities} = mobile) do
    abilities
    |> Enum.filter(&(&1["kind"] == "heal"))
    |> useable(mobile)
  end

  def on_cooldown?(%Mobile{effects: effects} = mobile, %{"cooldown" => _, "name" => name} = ability) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == name)) or on_global_cooldown?(mobile, ability)
  end
  def on_cooldown?(%Mobile{} = mobile, %{} = ability) do
    on_global_cooldown?(mobile, ability)
  end

  def on_global_cooldown?(%Mobile{},          %{"ignores_global_cooldown" => true}), do: false
  def on_global_cooldown?(%Mobile{} = mobile, %{}), do: on_global_cooldown?(mobile)
  def on_global_cooldown?(%Mobile{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == :global))
  end

  def apply_cooldown(%Mobile{} = mobile, %{"name" => "attack"} = ability) do
    if gc = global_cooldown(ability, mobile) do
      Systems.Effect.add(mobile, %{"cooldown" => :attack}, gc)
    else
      mobile
    end
  end

  def apply_cooldown(%Mobile{} = mobile, %{"cooldown" => cooldown, "name" => name} = ability) do
    mobile
    |> Systems.Effect.add(%{"cooldown" => name,
                            "expiration_message" => "#{capitalize_first(name)} is ready for use again."},
                          cooldown)
    |> apply_cooldown(Map.delete(ability, "cooldown"))
  end

  def apply_cooldown(%Mobile{} = mobile, %{} = ability) do
    if gc = global_cooldown(ability, mobile) do
      Systems.Effect.add(mobile, %{"cooldown" => :global}, gc)
    else
      mobile
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

  def global_cooldown(%{"ignores_global_cooldown" => true}, %Mobile{}), do: nil
  def global_cooldown(%{"global_cooldown" => gc}, %Mobile{effects: effects}) do
    speed_mods = effects
                 |> Map.values
                 |> Enum.filter(&(Map.has_key?(&1, "speed")))
                 |> Enum.map(&(Map.get(&1, "speed")))

    gc * speed_modifier(speed_mods)
  end
  def global_cooldown(%{}, %Mobile{}), do: nil

  def speed_modifier([]), do: 1
  def speed_modifier(speed_mods) do
    count = length(speed_mods)

    Enum.sum(speed_mods) / count / 100
  end

  def dodged?(%Mobile{} = mobile, %{"accuracy_stats" => stats}, %Mobile{} = attacker) do
    accuracy =
      stats
      |> Enum.reduce(0, fn(stat, total) ->
           total + Mobile.attribute(attacker, String.to_existing_atom(stat))
         end)
      |> div(length(stats))
      |> Kernel.+(Mobile.effect_bonus(mobile, "Accuracy"))

    dodge = Mobile.agility(mobile) + Mobile.effect_bonus(mobile, "Dodge")

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
    mobile = Room.get_mobile(room, target_ref)
    ability_user = Room.get_mobile(room, caster_ref)

    if dodged?(mobile, ability, ability_user) do

      Enum.each(room.mobiles, fn {ref, room_mobile} ->
        cond do
          ref == ability_user.ref ->
            user_message = interpolate(ability["dodge_message"]["user"], %{"user" => ability_user, "target" => mobile})
            Mobile.send_scroll(room_mobile, "<p><span class='dark-cyan'>#{user_message}</span></p>")
          ref == mobile.ref ->
            target_message = interpolate(ability["dodge_message"]["target"], %{"user" => ability_user, "target" => mobile})
            Mobile.send_scroll(room_mobile, "<p><span class='dark-cyan'>#{target_message}</span></p>")
          :else ->
            spectator_message = interpolate(ability["dodge_message"]["spectator"], %{"user" => ability_user, "target" => mobile})
            Mobile.send_scroll(room_mobile, "<p><span class='dark-cyan'>#{spectator_message}</span></p>")
        end
      end)

      2000
      |> :rand.uniform
      |> :erlang.send_after(self, {:think, target_ref})

      Room.update_mobile(room, target_ref, fn target ->
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
      Room.update_mobile(room, target_ref, fn target ->
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
    target = Room.get_mobile(room, target_ref)
    caster = Room.get_mobile(room, caster_ref)

    min_damage = trunc(80 * damage / 100)  + Mobile.effect_bonus(caster, "increase min damage")
    max_damage = trunc(120 * damage / 100) + Mobile.effect_bonus(caster, "increase max damage")
    damage = min_damage..max_damage |> Enum.random

    damage = Mobile.reduce_damage(target, damage, ability["instant_effects"]["mitigated_by"])

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
    if target = Room.get_mobile(room, target_ref) do
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
      Room.update_mobile(room, target_ref, fn target ->
        target = Map.put(target, :hp, target.hp - damage)

        if target_ref == caster_ref do
          target
        else
          enmity = trunc(damage * Map.get(effects, "hate_multiplier", 1.0))
          put_in(target.hate, Map.update(target.hate, caster_ref, damage, fn(hate) -> hate + enmity end))
        end
      end)

    room =
      if target = Room.get_mobile(room, target_ref) do
        chance = trunc((damage / target.hp) * 100)

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
    |> Room.update_mobile(target_ref, fn mobile ->
         put_in(mobile.hp, min(mobile.max_hp, mobile.hp + heal))
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "heal"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"heal_mana" => heal} = effects, caster_ref) do
    room
    |> Room.update_mobile(target_ref, fn mobile ->
         put_in(mobile.mana, min(mobile.max_mana, mobile.mana + heal))
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "heal_mana"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"script" => script} = effects, caster_ref) do
    script = ApathyDrive.Script.find(script)

    room
    |> Room.update_mobile(target_ref, fn mobile ->
         ApathyDrive.Script.execute(room, mobile, script)
       end)
    |> apply_instant_effects(target_ref, Map.delete(effects, "script"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"remove abilities" => abilities} = effects, caster_ref) do
    room
    |> Room.update_mobile(target_ref, fn mobile ->
         Enum.reduce(abilities, mobile, fn(ability_id, updated_mobile) ->
           Systems.Effect.remove_oldest_stack(updated_mobile, ability_id)
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

    mobile = Room.get_mobile(room, target_ref)

    room = ApathyDrive.Commands.Move.execute(room, mobile, room_exit)

    apply_instant_effects(room, target_ref, Map.delete(effects, "teleport"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{"dispel" => effect_types} = effects, caster_ref) do
    room
    |> Room.update_mobile(target_ref, fn mobile ->
         Enum.reduce(effect_types, mobile, fn(type, updated_monster) ->
           effects_with_type =
             mobile.effects
             |> Map.keys
             |> Enum.filter(fn(key) ->
                  effect = mobile.effects[key]
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
      Room.update_mobile(room, target_ref, fn mobile ->
        Map.put(mobile, :hp, 0)
      end)

    apply_instant_effects(room, target_ref, Map.delete(effects, "kill"), caster_ref)
  end
  def apply_instant_effects(%Room{} = room, target_ref, %{} = effects, caster_ref) do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.send_scroll("<p><span class='red'>unrecognized instant effects: #{inspect Map.keys(effects)}</span></p>")

    apply_instant_effects(room, target_ref, %{}, caster_ref)
  end

  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, damage, _crit_tables) when damage < 1, do: room
  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, _damage, nil), do: room
  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, _damage, []), do: room
  def apply_criticals(%Room{} = room, caster_ref, target_ref, damage, crit_tables) do
    damage
    |> ApathyDrive.Crits.abilities(crit_tables)
    |> Enum.reduce(room, fn
         %{"kind" => kind} = ability, updated_room when kind in ["attack", "curse"] ->
           execute(updated_room, caster_ref, ability, [target_ref])
         %{} = ability, updated_room ->
           execute(updated_room, caster_ref, ability, "")
       end)
  end

  def display_cast_message(room, target_ref, %{"cast_message" => _, "instant_effects" => %{"heal" => heal}} = ability, caster_ref) do
    target = Room.get_mobile(room, target_ref)
    caster = Room.get_mobile(room, caster_ref)

    cast_messages = cast_messages(ability, caster, target, %{"amount" => heal})

    room.mobiles
    |> Enum.each(fn {ref, mobile} ->
         cond do
           ref == caster_ref ->
             Mobile.send_scroll(mobile, cast_messages["user"])
           ref == target_ref ->
             Mobile.send_scroll(mobile, cast_messages["target"])
           true ->
             Mobile.send_scroll(mobile, cast_messages["spectator"])
         end
       end)
  end

  def display_cast_message(room, target_ref, %{"cast_message" => _, "instant_effects" => %{"damage" => damage}} = ability, caster_ref) do
    target = Room.get_mobile(room, target_ref)
    caster = Room.get_mobile(room, caster_ref)

    cast_messages = cast_messages(ability, caster, target, %{"amount" => damage})

    room.mobiles
    |> Enum.each(fn {ref, mobile} ->
         cond do
           ref == caster_ref ->
             Mobile.send_scroll(mobile, cast_messages["user"])
           ref == target_ref ->
             Mobile.send_scroll(mobile, cast_messages["target"])
           true ->
             Mobile.send_scroll(mobile, cast_messages["spectator"])
         end
       end)
  end

  def display_cast_message(room, target_ref, %{"cast_message" => _} = ability, caster_ref) do
    target = Room.get_mobile(room, target_ref)
    caster = Room.get_mobile(room, caster_ref)

    cast_messages = cast_messages(ability, caster, target)

    room.mobiles
    |> Enum.each(fn {ref, mobile} ->
         cond do
           ref == caster_ref ->
             Mobile.send_scroll(mobile, cast_messages["user"])
           ref == target_ref ->
             Mobile.send_scroll(mobile, cast_messages["target"])
           true ->
             Mobile.send_scroll(mobile, cast_messages["spectator"])
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

    Room.update_mobile(room, target_ref, fn mobile ->
      mobile
      |> Systems.Effect.add(effects, Map.get(ability, "duration", 0))
      |> Mobile.send_scroll("<p><span class='#{Ability.color(ability)}'>#{effects["effect_message"]}</span></p>")
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

  def affects_target?(%Mobile{flags: _monster_flags}, %{"flags" => nil}), do: true
  def affects_target?(%Mobile{flags: monster_flags}, %{"flags" => ability_flags}) do
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
  def affects_target?(%Mobile{flags: _monster_flags}, %{}), do: true
  def affects_target?(nil, %{}), do: false

end
