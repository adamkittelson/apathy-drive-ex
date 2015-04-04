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
  def color(%Ability{kind: "curse"}),       do: "red"
  def color(%Ability{kind: "area attack"}), do: "red"
  def color(%Ability{kind: "area curse"}),  do: "red"
  def color(%Ability{kind: _}), do: "blue"

  def prep_message(message, %Ability{} = ability, %Monster{} = user, %Monster{} = target, interpolations) do
    message = message
              |> interpolate(Map.merge(%{"user" => user, "target" => target}, interpolations))
              |> capitalize_first
    "<p><span class='#{color(ability)}'>#{message}</span></p>"
  end

  def cast_messages(%Ability{} = ability,
                    %Monster{} = user,
                    %Monster{} = target,
                    interpolations \\ %{}) do
    %{
      "user"      => prep_message(ability.properties["cast_message"]["user"],      ability, user, target, interpolations),
      "target"    => prep_message(ability.properties["cast_message"]["target"],    ability, user, target, interpolations),
      "spectator" => prep_message(ability.properties["cast_message"]["spectator"], ability, user, target, interpolations)
    }
  end

  def scale_ability(%Monster{} = monster, %Ability{} = ability) do
    ability = scale_duration_effects(monster, ability)
    ability = scale_instant_effects(monster, ability)
    scale_duration(monster, ability)
  end

  def scale_duration_effects(%Monster{} = monster,
                             %Ability{properties: %{"duration_effects" => effects}} = ability) do
    effects = scale_effects(monster, effects)

    properties = Map.put(ability.properties, "duration_effects", effects)

    Map.put(ability, :properties, properties)
  end
  def scale_duration_effects(%Monster{}, %Ability{} = ability) do
    ability
  end

  def scale_instant_effects(%Monster{} = monster,
                            %Ability{properties: %{"instant_effects" => effects}} = ability) do

    effects = scale_effects(monster, effects)

    properties = Map.put(ability.properties, "instant_effects", effects)

    Map.put(ability, :properties, properties)
  end
  def scale_instant_effects(%Monster{}, %Ability{} = ability) do
    ability
  end

  def scale_duration(%Monster{},
                     %Ability{properties: %{"duration" => duration}} = ability)
                     when is_integer(duration) do
    ability
  end

  def scale_duration(%Monster{} = monster,
                     %Ability{properties: %{"duration" => %{} = duration}} = ability) do
    cap  = Map.get(duration, "cap", :infinity)
    base = Map.get(duration, "base")

    duration = duration
               |> Map.drop(["cap", "base"])
               |> Map.keys
               |> Enum.reduce(base, fn(skill_name, total) ->
                    skill = Monster.modified_skill(monster, skill_name)

                    increase = if duration["every"] do
                      trunc(skill / duration["every"]) * duration["increase"]
                    else
                      0
                    end

                    min(total + increase, cap)
                  end)

    properties = Map.put(ability.properties, "duration", duration)

    Map.put(ability, :properties, properties)
  end

  def scale_duration(%Monster{}, %Ability{} = ability) do
    ability
  end

  def scale_effects(%Monster{} = monster, effects) do
    effects
    |> Map.keys
    |> Enum.reduce(%{}, fn(effect_name, scaled_effects) ->
         scaled_effect = scale_effect(monster, effect_name, effects[effect_name])
         Map.put(scaled_effects, effect_name, scaled_effect)
       end)
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

  def scale_effect(%Monster{}, _effect_name, %{"base_min" => base_min, "base_max" => base_max}) do
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

  def execute(%Monster{mana: mana} = monster,
              %Ability{properties: %{"mana_cost" => cost}}, _) when cost > mana do
    monster
    |> Monster.send_scroll("<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
  end

  def execute(%Monster{} = monster, %Ability{} = ability, "") do
    execute(monster, ability, monster)
  end

  def execute(%Monster{} = monster, %Ability{} = ability, target) when is_binary(target) do
    target_monster = monster
                     |> Monster.find_room
                     |> find_monster_in_room(target, monster)

    case target_monster do
      nil ->
        Monster.send_scroll(monster, "<p><span class='red'>You don't see #{target} here.</span></p>")
      target ->
        execute(monster, ability, target)
    end
  end

  def execute(%Monster{} = monster, %Ability{} = ability, target) when is_pid(target) do
    execute(monster, ability, Monster.value(target))
  end

  def execute(%Monster{} = monster, %Ability{} = ability, %Monster{} = target) do
    if Monster.on_global_cooldown?(monster) do
      Monster.send_scroll(monster, "<p><span class='dark-cyan'>You can't do that yet.</p>")
    else
      send(target.pid, {:apply_ability, scale_ability(monster, ability), monster})
      send(self, :think)

      gc = global_cooldown(ability, monster)

      if gc do
        Systems.Effect.add(monster, %{"cooldown" => :global, "expiration_message" => "You are ready to act again."}, gc)
      else
        monster
      end |> Map.put(:mana, monster.mana - Map.get(ability.properties, "mana_cost", 0))
          |> Systems.Prompt.update
    end
  end

  def global_cooldown(%Ability{global_cooldown: :weapon_speed} = ability, %Monster{} = monster) do
    cost = (ability.properties["weapon_speed"] * 1000) / (((((monster.level * 5) + 45) * (Monster.modified_stat(monster, "agility") + 150)) * 1500) / 9000.0)

    4 / (1000.0 / round(cost * ((trunc(trunc(Monster.current_encumbrance(monster) / Monster.max_encumbrance(monster)) * 100) / 2) + 75) / 100.0))
  end
  def global_cooldown(%Ability{global_cooldown: gc}, %Monster{}), do: gc

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
      monster = put_in(monster.hate, HashDict.update(monster.hate, ability_user.pid, 1, fn(hate) -> hate + 1 end))
    else
      ability = put_in(ability.properties["dodgeable"], false)
      apply_ability(monster, ability, ability_user)
    end
  end
  def apply_ability(%Monster{} = monster, %Ability{} = ability, %Monster{} = ability_user) do
    ability = reduce_damage(ability, monster)

    2000
    |> :random.uniform
    |> :erlang.send_after(self, :think)

    monster
    |> display_cast_message(ability, ability_user)
    |> apply_instant_effects(ability.properties["instant_effects"], ability_user)
    |> add_duration_effects(ability)
  end

  def reduce_damage(%Ability{properties:
                             %{"damage_type" => damage_type,
                               "instant_effects" => %{"damage" => damage}}} = ability,
                    %Monster{} = monster) do

    damage = Monster.reduce_damage(monster, damage, damage_type)

    put_in(ability.properties["instant_effects"]["damage"], damage)
  end
  def reduce_damage(%Ability{} = ability, _monster), do: ability

  def apply_instant_effects(%Monster{} = monster, nil, _ability_user), do: monster
  def apply_instant_effects(%Monster{} = monster, %{} = effects, _ability_user) when map_size(effects) == 0, do: monster
  def apply_instant_effects(%Monster{} = monster, %{"damage" => damage} = effects, %Monster{} = ability_user) do
    monster = put_in(monster.hp, monster.hp - damage)
    monster = put_in(monster.hate, HashDict.update(monster.hate, ability_user.pid, damage, fn(hate) -> hate + damage end))

    apply_instant_effects(monster, Map.delete(effects, "damage"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"heal" => heal} = effects, ability_user) do
    monster = put_in(monster.hp, min(Monster.max_hp(monster), monster.hp + heal))

    apply_instant_effects(monster, Map.delete(effects, "heal"), ability_user)
  end
  def apply_instant_effects(%Monster{} = monster, %{"script" => script} = effects, ability_user) do
    monster = Systems.Script.execute(script, monster)

    apply_instant_effects(monster, Map.delete(effects, "script"), ability_user)
  end

  def display_cast_message(%Monster{} = monster,
                           %Ability{properties:
                             %{"instant_effects" => %{"heal" => heal}}} = ability,
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
                             %{"instant_effects" => %{"damage" => damage}}} = ability,
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

  def affects_target?(%Monster{flags: []}, %Ability{}), do: true
  def affects_target?(%Monster{}, %Ability{flags: []}), do: true
  def affects_target?(%Monster{flags: monster_flags}, %Ability{flags: ability_flags}) do
    cond do
      Enum.member?(ability_flags, "affects living") and Enum.member?(monster_flags, "non-living") ->
        false
      Enum.member?(ability_flags, "affects animals") and Enum.member?(monster_flags, "animal") ->
        false
      Enum.member?(ability_flags, "affects undead") and Enum.member?(monster_flags, "undead") ->
        false
      Enum.member?(ability_flags, "poison") and Enum.member?(monster_flags, "poison immunity") ->
        false
      true ->
        true
    end
  end

end
