defmodule ApathyDrive.Ability do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{PubSub, Mobile, TimerManager, Ability}
  import Systems.Text
  import ApathyDrive.TimerManager, only: [seconds: 1]

  schema "abilities" do
    field :properties,      ApathyDrive.JSONB

    has_many :rooms, Room

    timestamps
  end

  @required_fields ~w(properties)
  @optional_fields ~w()

  def find(id) do
    ApathyDrive.Repo.get(__MODULE__, id)
    |> Map.get(:properties)
  end

  def changeset(model, params \\ :empty) do
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

  def scale_effect(%Mobile{} = mobile, effect_name, %{"scaling" => scaling} = effect) do
    cap_min = Map.get(effect, "cap_min", :infinity)
    cap_max = Map.get(effect, "cap_max", :infinity)

    effect = scaling
             |> Map.keys
             |> Enum.reduce(effect, fn(skill_name, effect) ->
                  skill = if skill_name == "level" do
                    case mobile do
                      %Mobile{spirit: %Spirit{level: level}} ->
                        level
                      %Mobile{level: level} ->
                        level
                    end
                  else
                    Mobile.modified_skill(mobile, skill_name)
                  end


                  min = if scaling[skill_name]["min_every"] do
                    trunc(skill / scaling[skill_name]["min_every"]) * scaling[skill_name]["min_increase"]
                  else
                    0
                  end

                  max = if scaling[skill_name]["max_every"] do
                    trunc(skill / scaling[skill_name]["max_every"]) * scaling[skill_name]["max_increase"]
                  else
                    0
                  end

                  effect
                  |> update_in(["base_min"], fn(base_min) -> min(base_min + min, cap_min) end)
                  |> update_in(["base_max"], fn(base_max) -> min(base_max + max, cap_max) end)
                end)
             |> Map.drop(["scaling"])
    scale_effect(mobile, effect_name, effect)
  end

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

  def scale_effect(%Mobile{}, _effect_name, %{"potency" => potency}) do
    potency
  end

  def scale_effect(%Mobile{}, _effect_name, effect), do: effect

  def find_mobile_in_room(room_id, string, %Mobile{pid: pid} = mobile) do
    PubSub.subscribers("rooms:#{room_id}:mobiles")
    |> Enum.map(fn(mobile_pid) ->
         if mobile_pid == pid do
           mobile
         else
           mobile_pid
         end
       end)
    |> Systems.Match.one(:name_contains, string)
  end

  def find_other_mobile_in_room(room_id, string, %Mobile{pid: pid}) do
    PubSub.subscribers("rooms:#{room_id}:mobiles", [pid])
    |> Systems.Match.one(:name_contains, string)
  end

  def alignment_enemies(%Mobile{} = mobile) do
    case mobile.alignment do
      "good" ->
        PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:neutral") ++
        PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:evil")
      "neutral" ->
        PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:good") ++
        PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:evil")
      "evil" ->
        PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:neutral") ++
        PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:good")
    end
  end

  def wrap_target(nil),    do: []
  def wrap_target(%Monster{pid: pid}), do: [pid]
  def wrap_target(target), do: [target]

  def reject(%Monster{pid: target_pid}, %Monster{pid: pid}) when pid == target_pid, do: nil
  def reject(%Monster{pid: target_pid}, %Monster{}), do: target_pid
  def reject(target_pid, %Monster{pid: pid}) when pid == target_pid, do: nil
  def reject(target_pid, %Monster{}), do: target_pid

  def local_hated_targets(%Monster{} = monster) do
    monster
    |> Monster.local_hated_targets
    |> Map.values
  end

  def targets(%Mobile{} = mobile, %{"kind" => "attack"}, "") do
    mobile
    |> Mobile.aggro_target
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "attack"}, target) when is_binary(target) do
    mobile.room_id
    |> find_other_mobile_in_room(target, mobile)
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "blessing"}, "") do
    mobile.pid
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind": "blessing"}, target) when is_binary(target) do
    mobile.room_id
    |> find_mobile_in_room(target, mobile)
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "curse"}, "") do
    mobile
    |> Mobile.aggro_target
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "curse"}, target) when is_binary(target) do
    mobile.room_id
    |> find_other_mobile_in_room(target, mobile)
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "heal"}, "") do
    mobile.pid
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "heal"}, target) when is_binary(target) do
    mobile.room_id
    |> find_mobile_in_room(target, mobile)
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "room attack"}, target) when is_binary(target) do
    Enum.uniq(alignment_enemies(mobile) ++ local_hated_targets(mobile))
  end

  def targets(%Mobile{} = mobile, %{"kind" => "room blessing"}, target) when is_binary(target) do
    Enum.uniq(PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")) --
    local_hated_targets(mobile)
  end

  def targets(%Mobile{} = mobile, %{"kind" => "room curse"}, target) when is_binary(target) do
    Enum.uniq(alignment_enemies(mobile) ++ local_hated_targets(mobile))
  end

  def targets(%Mobile{} = mobile, %{"kind" => "room heal"}, target) when is_binary(target) do
    Enum.uniq(PubSub.subscribers("rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")) --
    local_hated_targets(mobile)
  end

  def targets(%Mobile{} = mobile, %{"kind" => "utility"}, "") do
    mobile.pid
    |> wrap_target
  end

  def targets(%Mobile{} = mobile, %{"kind" => "utility"}, target) when is_binary(target) do
    mobile.room_id
    |> find_mobile_in_room(target, mobile)
    |> wrap_target
  end

  def interpolation_data(target, %Mobile{pid: pid} = mobile) when target == pid do
    Mobile.interpolation_data(mobile)
  end

  def interpolation_data(target, %Mobile{}) do
    Mobile.interpolation_data(target)
  end

  def display_pre_cast_message(%Mobile{} = mobile, %{"pre-cast_message" => _} = ability, targets) do
    target = targets
             |> List.first
             |> interpolation_data(mobile)

    cast_messages = cast_messages(ability, mobile, target, %{}, "pre-cast_message")

    Mobile.send_scroll(mobile, cast_messages["user"])

    PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [mobile])
    |> Enum.each(&(Mobile.send_scroll(&1, cast_messages["spectator"])))

  end
  def display_pre_cast_message(%Mobile{}, %{}, _targets), do: nil

  def can_execute?(%Mobile{} = mobile, ability) do
    cond do
      on_cooldown?(mobile, ability) ->
        Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>You can't do that yet.</p>")
        false
      Mobile.confused(mobile) ->
        false
      Mobile.silenced(mobile, ability) ->
        false
      true ->
        true
    end
  end

  def execute(%Mobile{timers: timers} = mobile, %{"cast_time" => time} = ability, target) do
    if can_execute?(mobile, ability) do
      if ref = Map.get(timers, :cast_timer) do
        Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You interrupt your spell.</span></p>")
        :erlang.cancel_timer(ref)
      end

      Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You begin your casting.</span></p>")

      TimerManager.send_after(mobile, {:cast_timer, time |> seconds, {:timer_cast_ability, %{ability: ability, timer: time, target: target}}})
    else
      mobile
    end
  end

  def execute(%Mobile{} = mobile, %{} = ability, target) when is_binary(target) do
    if can_execute?(mobile, ability) do
      case targets(mobile, ability, target) do
        [] ->
          Mobile.send_scroll(mobile, "<p><span class='red'>You don't see them here.</span></p>")
        targets ->
          execute(mobile, ability, targets)
      end
    else
      mobile
    end
  end

  def execute(%Mobile{} = mobile, %{"multi-cast" => times} = ability, targets) do
    ability = Map.delete(ability, "multi-cast")

    2..times |> Enum.each(fn(_) ->
      ability = ability
                |> Map.delete("pre-cast_message")
                |> Map.put("global_cooldown", nil)

      send(self, {:execute_ability, ability, targets})
    end)

    execute(mobile, ability, targets)
  end

  def execute(%Mobile{mana: mana} = mobile,
              %{"ignores_global_cooldown" => true, "mana_cost" => cost}, _) when cost > mana do
    mobile
  end

  def execute(%Mobile{mana: mana} = mobile,
              %{"mana_cost" => cost}, _) when cost > mana do
    mobile
    |> Mobile.send_scroll("<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
  end

  def execute(%Mobile{} = mobile, %{} = ability, targets) do
    send(self, :think)

    display_pre_cast_message(mobile, ability, targets)

    if mobile.combo && mobile.combo == get_in(ability, ["combo", "previous"]) do
      ability =
        ability
        |> Map.merge(ability["combo"])
        |> Map.delete("previous")
    end

    mobile = mobile
             |> apply_cooldown(ability)
             |> Map.put(:mana, mobile.mana - Map.get(ability, "mana_cost", 0))

    if ability["set_combo"] do
      mobile =
        mobile
        |> Map.put(:combo, ability["set_combo"])
    end

    Mobile.update_prompt(mobile)

    ability = scale_ability(mobile, nil, ability)

    Enum.each(targets, fn(target) ->
      send(target, {:apply_ability, ability, mobile})
    end)

    mobile
  end

  def attack_abilities(%Mobile{abilities: abilities} = mobile) do
    abilities
    |> Enum.filter(&(&1["kind"] == "attack"))
    |> useable(mobile)
  end

  def bless_abilities(%Mobile{abilities: abilities} = mobile) do
    abilities
    |> Enum.filter(&(&1["kind"] == "blessing"))
    |> Enum.reject(&(Map.has_key?(&1, "cooldown")))
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

  def after_cast(%{"ability_user" => %Mobile{} = ability_user,
                   "ability" => after_cast_ability,
                   "chance" => chance}, targets) do
    if chance >= :rand.uniform(100) do
      execute_after_cast(ability_user, after_cast_ability, targets)
    end
  end
  def after_cast(%{"ability_user" => %Mobile{} = ability_user,
                   "ability"   => after_cast_ability}, targets) do
    execute_after_cast(ability_user, after_cast_ability, targets)
  end
  def after_cast(%{}, _targets), do: false

  def execute_after_cast(%Mobile{} = ability_user, after_cast_ability, targets) do
    ability =
      after_cast_ability
      |> find()
      |> Map.put("ignores_global_cooldown", true)
    send(ability_user.pid, {:execute_ability, ability, targets})
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
      |> +(Mobile.effect_bonus(mobile, "Accuracy"))

    dodge = Mobile.agility(mobile) + Mobile.effect_bonus(mobile, "Dodge")

    if dodge > 0 do
      chance = dodge_chance(dodge, accuracy)

      :random.uniform(100) < trunc(chance)
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

  def apply_ability(%Mobile{} = mobile, %{"dodge_message" => _, "accuracy_stats" => _} = ability, %Mobile{} = ability_user) do
    if dodged?(mobile, ability, ability_user) do

      user_message = interpolate(ability["dodge_message"]["user"], %{"user" => ability_user, "target" => mobile})
      Mobile.send_scroll(ability_user, "<p><span class='dark-cyan'>#{user_message}</span></p>")

      target_message = interpolate(ability["dodge_message"]["target"], %{"user" => ability_user, "target" => mobile})
      unless ability_user.pid == self do
        Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{target_message}</span></p>")
      end

      spectator_message = interpolate(ability["dodge_message"]["spectator"], %{"user" => ability_user, "target" => mobile})
      PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [mobile.pid, ability_user.pid])
      |> Enum.each(&(Mobile.send_scroll(&1, "<p><span class='dark-cyan'>#{spectator_message}</span></p>")))

      2000
      |> :random.uniform
      |> :erlang.send_after(self, :think)

      put_in(mobile.hate, Map.update(mobile.hate, ability_user.pid, 1, fn(hate) -> hate + 1 end))
    else
      ability =
        ability
        |> Map.drop(["dodge_message", "accuracy_stats"])

      apply_ability(mobile, ability, ability_user)
    end
  end
  def apply_ability(%Mobile{} = mobile, %{} = ability, %Mobile{} = ability_user) do

    mobile = if Enum.member?(["curse", "room curse"], ability["kind"]) do
      put_in(mobile.hate, Map.update(mobile.hate, ability_user.pid, 1, fn(hate) -> hate + 1 end))
    else
      mobile
    end

    ability = reduce_damage(ability, mobile, ability_user)

    2000
    |> :random.uniform
    |> :erlang.send_after(self, :think)

    ability = display_cast_message(mobile, ability, ability_user)

    mobile
    |> apply_instant_effects(ability["instant_effects"], ability_user)
    |> add_duration_effects(ability, ability_user)
  end

  def reduce_damage(%{"instant_effects" => %{"damage" => damage}} = ability,
                    %Mobile{} = mobile,
                    %Mobile{} = ability_user) do

    min_damage = trunc(80 * damage / 100)  + Mobile.effect_bonus(ability_user, "increase min damage")
    max_damage = trunc(120 * damage / 100) + Mobile.effect_bonus(ability_user, "increase max damage")
    damage = min_damage..max_damage |> Enum.random

    damage = Mobile.reduce_damage(mobile, damage, ability["instant_effects"]["mitigated_by"])

    put_in(ability, ["instant_effects", "damage"], max(damage, 1))
  end
  def reduce_damage(%{"instant_effects" => %{"drain" => drain}} = ability, mobile, ability_user) do
     reduce_damage(put_in(ability, ["instant_effects", "damage"], drain), mobile, ability_user)
  end
  def reduce_damage(%{} = ability, _mobile, _ability_user) do
    ability
  end

  def trigger_damage_shields(%Mobile{pid: pid}, %Mobile{pid: attacker_pid}) when pid == attacker_pid, do: nil
  def trigger_damage_shields(%Mobile{} = mobile, %Mobile{} = attacker) do
    mobile.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "damage shield")))
    |> Enum.each(fn(%{"damage shield" => damage, "damage shield message" => message, "damage shield type" => damage_type}) ->
         ability = %{"kind" => "attack", "flags" => [], "instant_effects" => %{"damage" => damage}, "damage_type" => damage_type, "cast_message" => message}

         send(attacker.pid, {:apply_ability, ability, mobile})
       end)
  end

  def apply_instant_effects(%Mobile{} = mobile, nil, _ability_user), do: mobile
  def apply_instant_effects(%Mobile{} = mobile, %{} = effects, _ability_user) when map_size(effects) == 0, do: mobile
  def apply_instant_effects(%Monster{} = monster, %{"drain" => _} = effects, ability_user) do
    send(ability_user.pid, {:apply_ability, %Ability{properties: %{"instant_effects" => %{"heal" => effects["damage"]}}}, monster})

    apply_instant_effects(monster, Map.delete(effects, "drain"), ability_user)
  end
  def apply_instant_effects(%Mobile{} = mobile, %{"damage" => damage} = effects, %Mobile{} = ability_user) do
    mobile = put_in(mobile.hp, mobile.hp - damage)

    unless mobile.pid == ability_user.pid do
      enmity = trunc(damage * Map.get(effects, "hate_multiplier", 1.0))

      mobile = put_in(mobile.hate, Map.update(mobile.hate, ability_user.pid, damage, fn(hate) -> hate + enmity end))
    end

    trigger_damage_shields(mobile, ability_user)

    effects =
      effects
      |> Map.delete("damage")
      |> Map.delete("hate_multiplier")

    apply_instant_effects(mobile, effects, ability_user)
  end
  def apply_instant_effects(%Mobile{} = mobile, %{"heal" => heal} = effects, ability_user) do
    mobile = put_in(mobile.hp, min(mobile.max_hp, mobile.hp + heal))

    apply_instant_effects(mobile, Map.delete(effects, "heal"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"heal_mana" => heal} = effects, ability_user) do
    monster = put_in(monster.mana, min(monster.max_mana, monster.mana + heal))

    apply_instant_effects(monster, Map.delete(effects, "heal_mana"), ability_user)
  end
  def apply_instant_effects(%Mobile{} = mobile, %{"script" => script} = effects, ability_user) do
    script = ApathyDrive.Script.find(script)
    mobile = ApathyDrive.Script.execute(script, mobile)

    apply_instant_effects(mobile, Map.delete(effects, "script"), ability_user)
  end
  def apply_instant_effects(%Mobile{} = mobile, %{"remove abilities" => abilities} = effects, ability_user) do
    mobile = Enum.reduce(abilities, mobile, fn(ability_id, updated_mobile) ->
      Systems.Effect.remove_oldest_stack(updated_mobile, ability_id)
    end)

    apply_instant_effects(mobile, Map.delete(effects, "remove abilities"), ability_user)
  end
  def apply_instant_effects(%Mobile{} = mobile, %{"teleport" => room_ids} = effects, ability_user) do
    destination = Enum.random(room_ids)
    send(self, {:move_to, destination})

    Commands.Look.look_at_room(mobile, destination)

    apply_instant_effects(mobile, Map.delete(effects, "teleport"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"dispel" => effect_types} = effects, ability_user) do
    monster = Enum.reduce(effect_types, monster, fn(type, updated_monster) ->
      effects_with_type = monster.effects
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
        Systems.Effect.remove(sub_updated_monster, ability_id)
      end)
    end)

    apply_instant_effects(monster, Map.delete(effects, "dispel"), ability_user)
  end
  def apply_instant_effects(%Mobile{} = mobile, %{} = effects, ability_user) do
    Mobile.send_scroll(mobile, "<p><span class='red'>unrecognized instant effects: #{inspect Map.keys(effects)}</span></p>")
    apply_instant_effects(mobile, %{}, ability_user)
  end

  def display_cast_message(%Mobile{} = mobile,
                           %{"cast_message" => _, "instant_effects" => %{"heal" => heal}} = ability,
                           %Mobile{} = ability_user) do

    cast_messages = cast_messages(ability, ability_user, mobile, %{"amount" => heal})

    Mobile.send_scroll(ability_user, cast_messages["user"])

    unless ability_user.pid == self do
      Mobile.send_scroll(mobile, cast_messages["target"])
    end

    PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [mobile.pid, ability_user.pid])
    |> Enum.each(&(Mobile.send_scroll(&1, cast_messages["spectator"])))

    ability
  end

  def display_cast_message(%Mobile{} = mobile,
                           %{"cast_message" => _, "instant_effects" => %{"damage" => damage}} = ability,
                           %Mobile{} = ability_user) do

    cast_messages = cast_messages(ability, ability_user, mobile, %{"amount" => damage})

    Mobile.send_scroll(ability_user, cast_messages["user"])

    unless ability_user.pid == self do
      Mobile.send_scroll(mobile, cast_messages["target"])
    end

    PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [mobile.pid, ability_user.pid])
    |> Enum.each(&(Mobile.send_scroll(&1, cast_messages["spectator"])))

    ability
  end

  def display_cast_message(%Mobile{} = mobile,
                           %{"cast_message" => _} = ability,
                           %Mobile{} = ability_user) do

    cast_messages = cast_messages(ability, ability_user, mobile)

    Mobile.send_scroll(ability_user, cast_messages["user"])

    unless ability_user.pid == self do
      Mobile.send_scroll(mobile, cast_messages["target"])
    end

    PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [mobile.pid, ability_user.pid])
    |> Enum.each(&(Mobile.send_scroll(&1, cast_messages["spectator"])))

    ability
  end

  def display_cast_message(%Mobile{}, %{} = ability, %Mobile{}), do: ability

  def add_duration_effects(%Mobile{} = mobile,
                           %{
                               "duration_effects" => %{
                                 "stack_key"   => _stack_key,
                                 "stack_count" => _stack_count
                               } = effects
                             } = ability,
                            ability_user) do

     if Map.has_key? effects, "after_cast" do
       effects = put_in effects, ["after_cast", "ability_user"], ability_user
     end

     mobile
     |> Systems.Effect.add(effects, Map.get(ability, "duration", 0))
     |> Mobile.send_scroll("<p><span class='#{Ability.color(ability)}'>#{effects["effect_message"]}</span></p>")
  end
  def add_duration_effects(%Mobile{} = mobile,
                           %{
                               "duration_effects" => effects
                             } = ability,
                             ability_user) do

    effects = effects
              |> Map.put("stack_key",   ability["name"])
              |> Map.put("stack_count", 1)

    ability = Map.put(ability, "duration_effects", effects)

    add_duration_effects(mobile, ability, ability_user)
  end
  def add_duration_effects(%Mobile{} = mobile, %{}, _ability_user), do: mobile

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

end
