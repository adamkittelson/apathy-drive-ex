defmodule Ability do
  use Ecto.Model
  import Systems.Text
  alias ApathyDrive.Repo
  alias ApathyDrive.PubSub

  schema "abilities" do
    field :name,            :string
    field :command,         :string
    field :kind,            :string
    field :description,     :string
    field :required_skills, ApathyDrive.JSONB
    field :properties,      ApathyDrive.JSONB
    field :keywords,        {:array, :string}, virtual: true
    field :flags,           {:array, :string}
    field :global_cooldown, :any, virtual: true, default: 4

    has_many :rooms, Room

    timestamps
  end

  after_load :set_keywords

  def set_keywords(%Ability{name: name} = ability) do
    Map.put(ability, :keywords, String.split(name))
  end

  def trainable do
    query = from a in Ability, where: not is_nil(a.required_skills), select: a
    Repo.all(query)
  end

  def useable(abilities, %Monster{mana: mana} = monster) do
    abilities
    |> Enum.reject(fn(ability) ->
         ability.properties["mana_cost"] && ability.properties["mana_cost"] > mana
       end)
    |> Enum.reject(fn(ability) ->
         Systems.Effect.max_stacks?(monster, ability)
       end)
  end

  def color(%Ability{kind: "attack"}),      do: "red"
  def color(%Ability{kind: "room attack"}), do: "red"
  def color(%Ability{kind: _}), do: "blue"

  def prep_message(nil, _, _, _, _), do: nil
  def prep_message(message, %Ability{} = ability, %Monster{} = user, %Monster{} = target, interpolations) do
    message = message
              |> interpolate(Map.merge(%{"user" => user, "target" => target}, interpolations))
              |> capitalize_first
    "<p><span class='#{color(ability)}'>#{message}</span></p>"
  end

  def cast_messages(%Ability{} = ability,
                    %Monster{} = user,
                    %Monster{} = target,
                    interpolations \\ %{},
                    message_key \\ "cast_message") do
    %{
      "user"      => prep_message(ability.properties[message_key]["user"],      ability, user, target, interpolations),
      "target"    => prep_message(ability.properties[message_key]["target"],    ability, user, target, interpolations),
      "spectator" => prep_message(ability.properties[message_key]["spectator"], ability, user, target, interpolations)
    }
  end

  def scale_ability(%Monster{} = monster, %Ability{properties: properties} = ability) do
    put_in ability.properties, scale_properties(monster, nil, properties)
  end

  def scale_properties(%Monster{} = monster, prop_name, %{"base_min" => _, "base_max" => _} = properties) do
    scale_effect(monster, prop_name, properties)
  end
  def scale_properties(%Monster{} = monster, _prop_name, %{} = properties) do
    properties
    |> Map.keys
    |> Enum.reduce(%{}, fn(key, map) ->
         Map.put(map, key, scale_properties(monster, key, properties[key]))
       end)
  end
  def scale_properties(%Monster{} = monster, _prop_name, properties) do
    properties
  end

  def scale_effect(%Monster{} = _monster, value) when is_number(value), do: value
  def scale_effect(%Monster{} = _monster, value) when is_binary(value), do: value

  def scale_effect(%Monster{} = monster, effect_name, %{"scaling" => scaling} = effect) do
    cap_min = Map.get(effect, "cap_min", :infinity)
    cap_max = Map.get(effect, "cap_max", :infinity)

    effect = scaling
             |> Map.keys
             |> Enum.reduce(effect, fn(skill_name, effect) ->
                  skill = Monster.modified_skill(monster, skill_name)

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
    scale_effect(monster, effect_name, effect)
  end

  def scale_effect(%Monster{} = monster, "damage", %{"base_min" => base_min, "base_max" => base_max}) do
    base_max = base_max + Monster.effect_bonus(monster, "increase max damage")
    base_min..base_max
    |> Enum.shuffle
    |> List.first
  end

  def scale_effect(%Monster{}, effect_name, %{"base_min" => base_min, "base_max" => base_max}) do
    base_min..base_max
    |> Enum.shuffle
    |> List.first
  end

  def scale_effect(%Monster{}, _effect_name, effect), do: effect

  def find_monster_in_room(room, string, %Monster{pid: pid} = monster) do
    PubSub.subscribers("rooms:#{room.id}:monsters")
    |> Enum.map(fn(monster_pid) ->
         if monster_pid == pid do
           monster
         else
           monster_pid
         end
       end)
    |> Systems.Match.one(:name_contains, string)
  end

  def find_other_monster_in_room(room, string, %Monster{pid: pid} = monster) do
    PubSub.subscribers("rooms:#{room.id}:monsters")
    |> Enum.reject(fn(monster_pid) ->
         monster_pid == pid
       end)
    |> Systems.Match.one(:name_contains, string)
  end

  def alignment_enemies(%Monster{} = monster) do
    case Monster.monster_alignment(monster) do
      "good" ->
        PubSub.subscribers("rooms:#{monster.room_id}:monsters:neutral") ++
        PubSub.subscribers("rooms:#{monster.room_id}:monsters:evil")
      "neutral" ->
        PubSub.subscribers("rooms:#{monster.room_id}:monsters:good") ++
        PubSub.subscribers("rooms:#{monster.room_id}:monsters:evil")
      "evil" ->
        PubSub.subscribers("rooms:#{monster.room_id}:monsters:neutral") ++
        PubSub.subscribers("rooms:#{monster.room_id}:monsters:good")
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

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "attack"} = ability, "") do
    monster
    |> Monster.aggro_target
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "attack"} = ability, target) when is_binary(target) do
    monster
    |> Monster.find_room
    |> find_other_monster_in_room(target, monster)
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "blessing"} = ability, "") do
    monster.pid
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "blessing"} = ability, target) when is_binary(target) do
    monster
    |> Monster.find_room
    |> find_monster_in_room(target, monster)
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "curse"} = ability, "") do
    monster
    |> Monster.aggro_target
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "curse"} = ability, target) when is_binary(target) do
    monster
    |> Monster.find_room
    |> find_other_monster_in_room(target, monster)
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "heal"} = ability, "") do
    monster.pid
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "heal"} = ability, target) when is_binary(target) do
    monster
    |> Monster.find_room
    |> find_monster_in_room(target, monster)
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "room attack"} = ability, target) when is_binary(target) do
    Enum.uniq(alignment_enemies(monster) ++ local_hated_targets(monster))
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "room blessing"} = ability, target) when is_binary(target) do
    Enum.uniq(PubSub.subscribers("rooms:#{monster.room_id}:monsters:#{Monster.monster_alignment(monster)}")) --
    local_hated_targets(monster)
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "room curse"} = ability, target) when is_binary(target) do
    Enum.uniq(alignment_enemies(monster) ++ local_hated_targets(monster))
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "room heal"} = ability, target) when is_binary(target) do
    Enum.uniq(PubSub.subscribers("rooms:#{monster.room_id}:monsters:#{Monster.monster_alignment(monster)}")) --
    local_hated_targets(monster)
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "utility"} = ability, "") do
    monster.pid
    |> wrap_target
  end

  def targets(%Monster{alignment: alignment} = monster, %Ability{kind: "utility"} = ability, target) when is_binary(target) do
    monster
    |> Monster.find_room
    |> find_monster_in_room(target, monster)
    |> wrap_target
  end

  def display_pre_cast_message(%Monster{} = monster, %Ability{properties: %{"pre-cast_message" => _}} = ability, targets) do
    target = targets
             |> List.first
             |> Monster.value

    cast_messages = cast_messages(ability, monster, target, %{}, "pre-cast_message")

    PubSub.broadcast!("rooms:#{monster.room_id}",
                      {:cast_message,
                        messages: cast_messages,
                        user: monster,
                        target: target})
  end
  def display_pre_cast_message(%Monster{} = monster, %Ability{} = ability, targets), do: nil

  def execute(%Monster{} = monster, %Ability{} = ability, target) when is_binary(target)do
    case targets(monster, ability, target) do
      [] ->
        Monster.send_scroll(monster, "<p><span class='red'>You don't see them here.</span></p>")
      targets ->
        execute(monster, ability, targets)
    end
  end

  def execute(%Monster{} = monster, %Ability{properties: %{"multi-cast" => times}} = ability, targets) do
    ability = put_in(ability.properties, Map.delete(ability.properties, "multi-cast"))

    2..times |> Enum.each(fn(_) ->
      ability = put_in(ability.properties, Map.delete(ability.properties, "pre-cast_message"))
                |> Map.put(:global_cooldown, nil)
      send(self, {:execute_ability, ability, targets})
    end)

    execute(monster, ability, targets)
  end

  # def execute(%Monster{mana: mana} = monster,
  #             %Ability{global_cooldown: nil, properties: %{"mana_cost" => cost}}, _) when cost > mana do
  #   monster
  # end
  #
  # def execute(%Monster{mana: mana} = monster,
  #             %Ability{properties: %{"mana_cost" => cost}}, _) when cost > mana do
  #   monster
  #   |> Monster.send_scroll("<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
  # end

  def execute(%Monster{} = monster, %Ability{} = ability, targets) do
    cond do
      Monster.on_cooldown?(monster, ability) ->
        Monster.send_scroll(monster, "<p><span class='dark-cyan'>You can't do that yet.</p>")
      Monster.confuse(monster) ->
        monster
      Monster.silenced(monster, ability) ->
        monster
      true ->
        send(self, :think)

        display_pre_cast_message(monster, ability, targets)

        monster = monster
                  |> apply_cooldown(ability)
                  |> Map.put(:mana, monster.mana - Map.get(ability.properties, "mana_cost", 0))
                  |> Systems.Prompt.update

        ability = scale_ability(monster, ability)

        Enum.each(targets, fn(target) ->
          send(target, {:apply_ability, ability, monster})
        end)

        after_cast(ability, targets)

        monster
    end
  end

  def apply_cooldown(%Monster{} = monster, %Ability{name: "attack"} = ability) do
    if gc = global_cooldown(ability, monster) do
      Systems.Effect.add(monster, %{"cooldown" => :attack}, gc)
    else
      monster
    end
  end

  def apply_cooldown(%Monster{} = monster, %Ability{} = ability) do
    if gc = global_cooldown(ability, monster) do
      Systems.Effect.add(monster, %{"cooldown" => :global, "expiration_message" => "You are ready to act again."}, gc)
    else
      monster
    end
  end

  def after_cast(%Ability{properties: %{"after_cast" => ability_id, "after_cast_chance" => chance}}, targets) do
    if chance >= :random.uniform(100) do
      execute_after_cast(ability_id, targets)
    end
  end
  def after_cast(%Ability{properties: %{"after_cast" => ability_id}}, targets) do
    execute_after_cast(ability_id, targets)
  end
  def after_cast(%Ability{}, targets), do: false

  def execute_after_cast(ability_id, targets) do
    if after_cast_ability = ApathyDrive.Repo.get(Ability, ability_id) do
      send(self, {:execute_ability, after_cast_ability, targets})
    end
  end

  def global_cooldown(%Ability{global_cooldown: gc}, %Monster{effects: effects}) do
    if gc do
      speed_mods = effects
                   |> Map.values
                   |> Enum.filter(&(Map.has_key?(&1, "speed")))
                   |> Enum.map(&(Map.get(&1, "speed")))

      gc * speed_modifier(speed_mods)
    end
  end

  def speed_modifier([]), do: 1
  def speed_modifier(speed_mods) do
    count = length(speed_mods)

    Enum.sum(speed_mods) / 100
  end

  def dodged?(%Monster{} = monster, %Ability{properties: %{"accuracy_skill" => accuracy_skill}}, %Monster{} = attacker) do
    dodge = Monster.modified_skill(monster, "dodge")
    accuracy = Monster.modified_skill(attacker, accuracy_skill)

    chance = 30
    if dodge > 0 do
      difference = dodge - accuracy
      chance = if difference > 0 do
        chance + difference * 0.2
      else
        chance + difference * 0.3
      end

      :random.uniform(100) < trunc(chance)
    else
      false
    end
  end

  def apply_ability(%Monster{} = monster, %Ability{properties: %{"dodgeable" => true}} = ability, %Monster{} = ability_user) do
    if dodged?(monster, ability, ability_user) do
      ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_dodged, messages: ability.properties["dodge_message"],
                                                                                  user: ability_user,
                                                                                  target: monster})
      put_in(monster.hate, HashDict.update(monster.hate, ability_user.pid, 1, fn(hate) -> hate + 1 end))
    else
      ability = put_in(ability.properties["dodgeable"], false)
      apply_ability(monster, ability, ability_user)
    end
  end
  def apply_ability(%Monster{} = monster, %Ability{} = ability, %Monster{} = ability_user) do

    monster = if Enum.member?(["curse", "room curse"], ability.kind) do
      put_in(monster.hate, HashDict.update(monster.hate, ability_user.pid, 1, fn(hate) -> hate + 1 end))
    else
      monster
    end

    ability = reduce_damage(ability, monster, ability_user)

    2000
    |> :random.uniform
    |> :erlang.send_after(self, :think)

    monster
    |> display_cast_message(ability, ability_user)
    |> apply_instant_effects(ability.properties["instant_effects"], ability_user)
    |> add_duration_effects(ability)
    |> Monster.save
  end

  def reduce_damage(%Ability{properties:
                             %{"damage_type" => damage_type,
                               "instant_effects" => %{"damage" => damage}}} = ability,
                    %Monster{} = monster,
                    %Monster{alignment: alignment}) do

    damage = Monster.reduce_damage(monster, damage, damage_type)

    damage = case alignment do
      "good" ->
        prgd = Monster.effect_bonus(monster, "protection from good")
        damage * (1 - prgd)
      "evil" ->
        prev = Monster.effect_bonus(monster, "protection from evil")
        damage * (1 - prev)
      _ ->
        damage
    end

    put_in(ability.properties["instant_effects"]["damage"], damage)
  end
  def reduce_damage(%Ability{properties:
                             %{"damage_type" => damage_type,
                               "instant_effects" => %{"drain" => drain}}} = ability, monster, ability_user) do
     reduce_damage(put_in(ability.properties["instant_effects"]["damage"], drain), monster, ability_user)
  end
  def reduce_damage(%Ability{} = ability, _monster, _ability_user), do: ability

  def trigger_damage_shields(%Monster{id: id}, %Monster{id: attacker_id}) when id == attacker_id, do: nil
  def trigger_damage_shields(%Monster{} = monster, %Monster{} = attacker) do
    monster.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "damage shield")))
    |> Enum.each(fn(%{"damage shield" => damage, "damage shield message" => message, "damage shield type" => damage_type}) ->
         ability = %Ability{kind: "attack", flags: [], properties: %{"instant_effects" => %{"damage" => damage}, "damage_type" => damage_type, "cast_message" => message}}

         send(attacker.pid, {:apply_ability, ability, monster})
       end)
  end

  def apply_instant_effects(%Monster{} = monster, nil, _ability_user), do: monster
  def apply_instant_effects(%Monster{} = monster, %{} = effects, _ability_user) when map_size(effects) == 0, do: monster
  def apply_instant_effects(%Monster{} = monster, %{"drain" => _} = effects, ability_user) do
    send(ability_user.pid, {:apply_ability, %Ability{properties: %{"instant_effects" => %{"heal" => effects["damage"]}}}, monster})

    apply_instant_effects(monster, Map.delete(effects, "drain"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"damage" => damage} = effects, %Monster{} = ability_user) do
    monster = put_in(monster.hp, monster.hp - damage)
    monster = put_in(monster.hate, HashDict.update(monster.hate, ability_user.pid, damage, fn(hate) -> hate + damage end))

    trigger_damage_shields(monster, ability_user)

    apply_instant_effects(monster, Map.delete(effects, "damage"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"heal" => heal} = effects, ability_user) do
    monster = put_in(monster.hp, min(monster.max_hp, monster.hp + heal))

    apply_instant_effects(monster, Map.delete(effects, "heal"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"heal_mana" => heal} = effects, ability_user) do
    monster = put_in(monster.mana, min(monster.max_mana, monster.mana + heal))

    apply_instant_effects(monster, Map.delete(effects, "heal_mana"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"script" => script} = effects, ability_user) do
    monster = Systems.Script.execute(script, monster)

    apply_instant_effects(monster, Map.delete(effects, "script"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"remove abilities" => abilities} = effects, ability_user) do
    monster = Enum.reduce(abilities, monster, fn(ability_id, updated_monster) ->
      Systems.Effect.remove_oldest_stack(updated_monster, ability_id)
    end)

    apply_instant_effects(monster, Map.delete(effects, "remove abilities"), ability_user)
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
  def apply_instant_effects(%Monster{} = monster, %{} = effects, ability_user) do
    IO.puts "unrecognized instant effects: #{inspect Map.keys(effects)}"
    apply_instant_effects(monster, %{}, ability_user)
  end

  def display_cast_message(%Monster{} = monster,
                           %Ability{properties:
                             %{"cast_message" => _, "instant_effects" => %{"heal" => heal}}} = ability,
                           %Monster{} = ability_user) do

    cast_messages = cast_messages(ability, ability_user, monster, %{"amount" => heal})

    PubSub.broadcast_from!(self, "rooms:#{monster.room_id}",
                                {:cast_message,
                                  messages: cast_messages,
                                  user: ability_user,
                                  target: monster})

    if ability_user.pid == self do
      Monster.send_scroll(monster, cast_messages["user"])
    else
      Monster.send_scroll(monster, cast_messages["target"])
    end
  end

  def display_cast_message(%Monster{} = monster,
                           %Ability{properties:
                             %{"cast_message" => _, "instant_effects" => %{"damage" => damage}}} = ability,
                           %Monster{} = ability_user) do

    cast_messages = cast_messages(ability, ability_user, monster, %{"amount" => damage})

    PubSub.broadcast_from!(self, "rooms:#{monster.room_id}",
                                {:cast_message,
                                  messages: cast_messages,
                                  user: ability_user,
                                  target: monster})

    if ability_user.pid == self do
      Monster.send_scroll(monster, cast_messages["user"])
    else
      Monster.send_scroll(monster, cast_messages["target"])
    end
  end

  def display_cast_message(%Monster{} = monster,
                           %Ability{properties: %{"cast_message" => _}} = ability,
                           %Monster{} = ability_user) do

    cast_messages = cast_messages(ability, ability_user, monster)

    PubSub.broadcast_from!(self, "rooms:#{monster.room_id}",
                                {:cast_message,
                                  messages: cast_messages,
                                  user: ability_user,
                                  target: monster})

    if ability_user.pid == self do
      Monster.send_scroll(monster, cast_messages["user"])
    else
      Monster.send_scroll(monster, cast_messages["target"])
    end
  end

  def display_cast_message(%Monster{} = monster, %Ability{}, %Monster{}), do: monster

  def add_duration_effects(%Monster{} = monster,
                           %Ability{
                             properties: %{
                               "duration_effects" => %{
                                 "stack_key"   => _stack_key,
                                 "stack_count" => _stack_count
                               }
                             }
                           } = ability) do

     monster
     |> Systems.Effect.add(ability.properties["duration_effects"],
                           ability.properties["duration"])
     |> Monster.send_scroll("<p><span class='#{Ability.color(ability)}'>#{ability.properties["duration_effects"]["effect_message"]}</span></p>")
  end
  def add_duration_effects(%Monster{} = monster,
                           %Ability{
                             properties: %{
                               "duration_effects" => effects
                             }
                           } = ability) do

    effects = effects
              |> Map.put("stack_key",   ability.name)
              |> Map.put("stack_count", 1)

    properties = Map.put(ability.properties, "duration_effects", effects)

    ability = Map.put(ability, :properties, properties)

    add_duration_effects(monster, ability)
  end
  def add_duration_effects(%Monster{} = monster, %Ability{}), do: monster

  def affects_target?(%Monster{flags: _monster_flags}, %Ability{flags: nil}), do: true
  def affects_target?(%Monster{flags: monster_flags}, %Ability{flags: ability_flags}) do
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

end
