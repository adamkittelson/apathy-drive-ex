defmodule Systems.Ability do
  use Systems.Reload
  import Systems.Text
  import Utility
  import BlockTimer
  alias Systems.Effect

  def execute(ability, entity, target) do
    if Systems.Cooldown.on_cooldown?(entity, ability[:name]) do
      send_message(entity, "scroll", "<p><span class='dark-cyan'>You can't use #{ability[:name]} for another #{Systems.Cooldown.cooldown_remaining(entity, ability[:name])} seconds.</span></p>")
    else
      if ability[:casting_time] do
        delay_execution(ability, entity, target)
      else
        execute(ability, entity, target, :verify_target)
      end
    end
  end

  def execute(ability, entity, target, :verify_target) do
    room = Parent.of(entity)

    case find_target(ability, room, entity, target) do
      target_entity ->
        execute(ability, entity, target_entity, :mana)
      _ ->
        send_message(entity, "scroll", "<p><span class='cyan'>Can't find #{target} here!  Your spell fails.</span></p>")
    end
  end

  def execute(ability, entity, target, :mana) do
    if ability[:mana_cost] do
      if Components.Mana.subtract(entity, ability[:mana_cost]) do
        ManaRegen.add(entity)
        entity
        |> Possession.possessor
        |> Systems.Prompt.update(entity)
        execute(ability, entity, target, :cooldown)
      else
        send_message(entity, "scroll", "<p><span class='dark-cyan'>You don't have enough mana.</span></p>")
      end
    else
      execute(ability, entity, target, :cooldown)
    end
  end

  def execute(ability, entity, target, :dodge) do
    if ability[:dodgeable] do
      skill = Skills.find(ability[:skill]).modified(entity)

      if Systems.Combat.dodge?(skill, target) do
        display_dodge_message(ability, entity, target)
      else
        execute(ability, entity, target, :parry)
      end
    else
      execute(ability, entity, target, :parry)
    end
  end

  def execute(ability, entity, target, :parry) do
    if ability[:parryable] && Systems.Limbs.wielding_weapon?(target) do
      skill = Skills.find(ability[:skill]).modified(entity)

      if Systems.Combat.parry?(skill, target) do
        display_parry_message(ability, entity, target)
      else
        execute(ability, entity, target, :healing)
      end
    else
      execute(ability, entity, target, :healing)
    end
  end

  def execute(ability, entity, target, :healing) do
    if Map.has_key?(ability, :healing) do
      :random.seed(:os.timestamp)
      amount = ability[:healing] |> Enum.into([]) |> Enum.shuffle |> List.first
      display_cast_message(ability, entity, target, %{"amount" => amount})
      Components.HP.add(target, amount)
      target
      |> Possession.possessor
      |> Systems.Prompt.update(target)
    end
    execute(ability, entity, target, :damage)
  end

  def execute(ability, entity, target, :damage) do
    if Map.has_key?(ability, :damage) do
      Systems.Combat.enrage(target, entity)
      {limb, damage, crit} = Systems.Damage.calculate_damage(ability, target)
      display_cast_message(ability, entity, target, %{"damage" => damage})
      display_crit_message(crit, entity, target)
      Systems.Crits.add_crit_effects(damage, target, crit[:effects])
      if crit[:effects][:kill] do
        Systems.Death.kill(target)
      else
        if damage && damage > 0 do
          damage_limb(target, limb, damage)
        end
      end
    else
      if !Map.has_key?(ability, :healing) do
        display_cast_message(ability, entity, target)
      end
    end
    execute(ability, entity, target, :apply_effects)
  end

  def execute(ability, entity, target, :apply_effects) do
    if ability[:effects] && Process.alive?(target) do
      Effect.add(target, ability[:effects], ability[:effects][:duration])
      if ability[:effects][:effect_message] do
        send_message(target, "scroll", "<p><span class='blue'>#{ability[:effects][:effect_message]}</span></p>")
      end
    end
    execute(ability, entity, target, :script)
  end

  def execute(ability, entity, target, :script) do
    if ability[:script] do
      Systems.Script.execute(ability[:script], entity)
    end
  end

  def execute(ability, entity, target, :cooldown) do
    if ability[:cooldown] do
      {:ok, timer} = apply_interval 1 |> seconds do
        Components.Effects.update(entity, fn(value) ->
          key = value
                |> Map.keys
                |> Enum.find(fn(key) ->
                     value[key][:cooldown] == ability[:name]
                   end)

          if key do
            update_in(value, [key, :cooldown_remaining], &(&1 - 1))
          else
            value
          end
        end)
      end
      effect = %{:timers       => [timer],
                 :cooldown     => ability[:name],
                 :cooldown_remaining => ability[:cooldown],
                 :wear_message => "You may now use #{ability[:name]} again."}
      Effect.add(entity, effect, ability[:cooldown])
    end

    if is_list(target) do
      display_area_cast_message(ability, entity)

      target
      |> Enum.each fn(target) ->
        execute(ability, entity, target, :dodge)
      end
    else
      execute(ability, entity, target, :dodge)
    end
  end

  def damage_limb(target, limb, amount) do
    case Systems.Damage.damage_limb(target, limb, amount) do
      :fatal ->
        Systems.Death.kill(target)
      _ ->
        Systems.Damage.do_damage(target, amount)
      end
  end

  def display_crit_message(nil, _, _), do: nil

  def display_crit_message(crit, entity, target) do
    opts = %{"user" => entity, "target" => target}
    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

      observer = Possession.possessed(character) || character

      cond do
        observer == entity ->
          send_message(character, "scroll", "<p><span class='red'>#{interpolate(crit[:user_message], opts) |> capitalize_first}</span></p>")
        observer == target ->
          send_message(character, "scroll", "<p><span class='red'>#{interpolate(crit[:target_message], opts) |> capitalize_first}</span></p>")
        true ->
          send_message(character, "scroll", "<p><span class='red'>#{interpolate(crit[:spectator_message], opts) |> capitalize_first}</span></p>")
      end
    end)

  end

  def display_cast_message(ability, entity, target, opts \\ %{}) do
    opts = Map.merge(opts, %{"user" => entity, "target" => target})
    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

      observer = Possession.possessed(character) || character

      cond do
        observer == entity ->
          send_message(character, "scroll", interpolate(ability[:user_message] || "", opts))
        observer == target ->
          send_message(character, "scroll", interpolate(ability[:target_message], opts))
        true ->
          send_message(character, "scroll", interpolate(ability[:spectator_message], opts))
      end
    end)
  end

  def display_area_cast_message(ability, entity, opts \\ %{}) do
    opts = Map.merge(opts, %{"user" => entity})
    Parent.of(entity)
    |> Systems.Room.living_in_room
    |> Enum.each(fn(monster) ->

      cond do
        monster == entity ->
          send_message(monster, "scroll", interpolate(ability[:area_user_message] || "", opts))
        true ->
          send_message(monster, "scroll", interpolate(ability[:area_spectator_message], opts))
      end
    end)
  end

  def display_dodge_message(ability, entity, target) do
    opts = %{"user" => entity, "target" => target}

    user_dodge_message     = ability[:user_dodge_message]      || "{{target}} dodges your attack!"
                             |> interpolate(opts)
                             |> capitalize_first

    target_dodge_message    = ability[:target_dodge_message]    || "You dodge {{user}}'s attack!"
                              |> interpolate(opts)
                              |> capitalize_first

    spectator_dodge_message = ability[:spectator_dodge_message] || "{{target}} dodges {{user}}'s attack!"
                              |> interpolate(opts)
                              |> capitalize_first

    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

      observer = Possession.possessed(character) || character

      cond do
        observer == entity ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{user_dodge_message}</span></p>")
        observer == target ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{target_dodge_message}</span></p>")
        true ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{spectator_dodge_message}</span></p>")
      end
    end)
  end

  def display_parry_message(ability, entity, target) do
    opts = %{"user" => entity, "target" => target}

    user_parry_message     = ability[:user_parry_message]      || "{{target}} parries your attack!"
                             |> interpolate(opts)
                             |> capitalize_first

    target_parry_message    = ability[:target_parry_message]    || "You parry {{user}}'s attack!"
                              |> interpolate(opts)
                              |> capitalize_first

    spectator_parry_message = ability[:spectator_parry_message] || "{{target}} parries {{user}}'s attack!"
                              |> interpolate(opts)
                              |> capitalize_first

    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

      observer = Possession.possessed(character) || character

      cond do
        observer == entity ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{user_parry_message}</span></p>")
        observer == target ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{target_parry_message}</span></p>")
        true ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{spectator_parry_message}</span></p>")
      end
    end)
  end

  def kill(entity, target) do
    opts = %{"user" => entity, "target" => target}
    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

      observer = Possession.possessed(character) || character

      cond do
        observer == entity ->
          send_message(character, "scroll", interpolate("<p>You just killed {{target}}!</p>", opts))
        observer == target ->
          send_message(character, "scroll", interpolate("<p>{{user}} just killed you!</p>", opts))
        true ->
          send_message(character, "scroll", interpolate("<p>{{user}} just killed {{target}}!</p>", opts))
      end
    end)
  end

  def delay_execution(ability, entity, target) do
    display_precast_message(ability, entity)

    {:ok, cast_timer} = apply_after(ability[:casting_time] |> seconds, ability[:env]) do
      execute(ability, entity, target, :verify_target)
    end

    Systems.Effect.add(entity, %{:stack_key => :cast_timer, :stack_count => 1, timers: [cast_timer]}, ability[:casting_time])
  end

  def display_precast_message(_ability, entity) do
    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

         observer = Possession.possessed(character) || character

         if observer == entity do
           send_message(entity, "scroll", "<p><span class='dark-cyan'>You begin your casting.</span></p>")
         else
           message = capitalize_first("#{Components.Name.value(entity)} begins casting a spell.")
           send_message(entity, "scroll", "<p><span class='dark-cyan'>#{message}</span></p>")
         end
       end)
  end

  def find_target(ability, room, entity, target) do
    case ability[:target] do
      "living" ->
        if to_string(target) == "" do
          entity
        else
          room
          |> Systems.Room.living_in_room
          |> Systems.Match.one(:name_contains, target)
        end
      "room" ->
        room
        |> Systems.Room.living_in_room
        |> Enum.reject(&(&1 == entity))
      _ ->
        nil
    end
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import BlockTimer

      def name(_caster), do: name
      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
        |> String.replace("_", " ")
      end

      def keywords do
        (name |> String.split)
      end

      def properties(caster) do
        %{
          env:              __ENV__,
        }
        |> apply_property(:casting_time, caster)
        |> apply_property(:target, caster)
        |> apply_property(:mana_cost, caster)
        |> apply_property(:damage, caster)
        |> apply_property(:user_message, caster)
        |> apply_property(:target_message, caster)
        |> apply_property(:spectator_message, caster)
        |> apply_property(:area_user_message, caster)
        |> apply_property(:area_spectator_message, caster)
        |> apply_property(:command, caster)
        |> apply_property(:effects, caster)
        |> apply_property(:dodgeable, caster)
        |> apply_property(:parryable, caster)
        |> apply_property(:magic_damage, caster)
        |> apply_property(:attack_damage, caster)
        |> apply_property(:melee_damage, caster)
        |> apply_property(:magic_healing, caster)
        |> apply_property(:cooldown, caster)
        |> apply_property(:name, caster)
        |> apply_property(:script, caster)
      end

      def effects,        do: nil
      def effects(caster) do
        %{}
        |> apply_property(:duration, caster)
        |> apply_property(:damage_increase, caster)
        |> apply_property(:wear_message, caster)
        |> apply_property(:stack_key, caster)
        |> apply_property(:stack_count, caster)
        |> apply_property(:dodge, caster)
        |> apply_property(:effect_message, caster)
        |> apply_property(:stun, caster)
        |> apply_property(:increase_skills, caster)
        |> finalize_effects
      end

      def finalize_effects(%{duration: _} = effects), do: effects
      def finalize_effects(_), do: nil

      def duration(entity \\ nil)
      def duration(entity), do: nil

      def cooldown(entity \\ nil)
      def cooldown(entity), do: nil

      def dodgeable(entity \\ nil)
      def dodgeable(entity), do: nil

      def parryable(entity \\ nil)
      def parryable(entity), do: nil

      def dodge(entity \\ nil)
      def dodge(entity), do: nil

      def stun(entity \\ nil)
      def stun(entity), do: nil

      def stack_key(entity \\ nil)
      def stack_key(entity), do: name

      def stack_count(entity \\ nil)
      def stack_count(entity), do: 1

      def effect_message(entity \\ nil)
      def effect_message(entity), do: nil

      def wear_message(entity \\ nil)
      def wear_message(entity) do
        "The effects of #{name} wear off."
      end

      def casting_time(entity \\ nil)
      def casting_time(entity), do: nil

      def target(entity \\ nil)
      def target(entity) do
        "living"
      end

      def mana_cost(entity \\ 0)
      def mana_cost(entity), do: 0

      def magic_damage(entity \\ nil)
      def magic_damage(entity), do: nil

      def magic_healing(entity \\ nil)
      def magic_healing(entity), do: nil

      def attack_damage(entity \\ nil)
      def attack_damage(entity), do: nil

      def melee_damage(entity \\ nil)
      def melee_damage(entity), do: nil

      def damage(entity \\ nil)
      def damage(entity), do: nil

      def damage_increase(entity \\ nil)
      def damage_increase(entity), do: nil

      def user_message(entity \\ nil)
      def user_message(entity), do: ""

      def target_message(entity \\ nil)
      def target_message(entity), do: ""

      def spectator_message(entity \\ nil)
      def spectator_message(entity), do: ""

      def area_user_message(entity \\ nil)
      def area_user_message(entity), do: ""

      def area_spectator_message(entity \\ nil)
      def area_spectator_message(entity), do: ""

      def required_skills(entity \\ nil)
      def required_skills(entity), do: nil

      def command(entity \\ nil)
      def command(entity), do: nil

      def increase_skills(entity \\ nil)
      def increase_skills(entity), do: nil

      def script(entity \\ nil)
      def script(entity), do: nil

      def execute(entity, target) do
        Systems.Ability.execute(properties(entity), entity, target)
      end

      def apply_property(nil, _, _), do: nil
      def apply_property(properties, :magic_damage, caster) do
        value = apply(__MODULE__, :magic_damage, []) || apply(__MODULE__, :magic_damage, [caster])
        if value do
          low..high = Systems.Damage.base_magic_damage(caster)

          damage = value
            |> Map.keys
            |> Enum.reduce(%{}, fn(damage_type, damage) ->
                 Map.put(damage, damage_type, (trunc(low * value[damage_type]))..(trunc(high * value[damage_type])))
               end)

          append_property(properties, :damage, damage)
        else
          properties
        end
      end
      def apply_property(properties, :attack_damage, caster) do
        value = apply(__MODULE__, :attack_damage, []) || apply(__MODULE__, :attack_damage, [caster])
        if value do
          low..high = Systems.Damage.base_attack_damage(caster)

          damage = value
            |> Map.keys
            |> Enum.reduce(%{}, fn(damage_type, damage) ->
                 Map.put(damage, damage_type, (trunc(low * value[damage_type]))..(trunc(high * value[damage_type])))
               end)

          append_property(properties, :damage, damage)
        else
          properties
        end
      end
      def apply_property(properties, :melee_damage, caster) do
        value = apply(__MODULE__, :melee_damage, []) || apply(__MODULE__, :melee_damage, [caster])

        if value do
          damage_increases = Components.Effects.damage_increases(caster)

          value = damage_increases
                  |> Enum.reduce(value, fn(damage_increase, attack) ->
                       damage_increase
                       |> Map.keys
                       |> Enum.reduce(value, fn(table, value) ->
                            update_in(value, [table], &((&1 || 0) + damage_increase[table]))
                          end)
                     end)

          low..high = Systems.Damage.base_attack_damage(caster)

          damage = value
            |> Map.keys
            |> Enum.reduce(%{}, fn(damage_type, damage) ->
                 Map.put(damage, damage_type, (trunc(low * value[damage_type]))..(trunc(high * value[damage_type])))
               end)

          append_property(properties, :damage, damage)
        else
          properties
        end
      end
      def apply_property(properties, :magic_healing, caster) do
        value = apply(__MODULE__, :magic_healing, []) || apply(__MODULE__, :magic_healing, [caster])
        if value do
          low..high = Systems.Damage.base_magic_damage(caster)

          append_property(properties, :healing, trunc(low * value)..trunc(high * value))
        else
          properties
        end
      end
      def apply_property(properties, :stun, caster) do
        value = apply(__MODULE__, :stun, []) || apply(__MODULE__, :stun, [caster])
        append_property properties, :stunned, value
      end
      def apply_property(properties, :increase_skills, caster) do
        value = apply(__MODULE__, :increase_skills, []) || apply(__MODULE__, :increase_skills, [caster])

        if value do
          value
          |> Map.keys
          |> Enum.reduce(properties, fn(skill_name, properties) ->
               append_property properties, skill_name, value[skill_name]
             end)
        else
          properties
        end

      end
      def apply_property(properties, property, caster) do
        value = apply(__MODULE__, property, []) || apply(__MODULE__, property, [caster])
        append_property properties, property, value
      end

      def append_property(properties, property, nil),   do: properties
      def append_property(properties, property, value), do: Map.put(properties, property, value)
      def help, do: nil

      defoverridable [required_skills: 0,
                      required_skills: 1,
                      casting_time: 0,
                      casting_time: 1,
                      target: 0,
                      target: 1,
                      mana_cost: 0,
                      mana_cost: 1,
                      damage: 0,
                      damage: 1,
                      user_message: 0,
                      user_message: 1,
                      target_message: 0,
                      target_message: 1,
                      spectator_message: 0,
                      spectator_message: 1,
                      area_user_message: 0,
                      area_user_message: 1,
                      area_spectator_message: 0,
                      area_spectator_message: 1,
                      command: 0,
                      command: 1,
                      duration: 0,
                      duration: 1,
                      damage_increase: 0,
                      damage_increase: 1,
                      wear_message: 0,
                      wear_message: 1,
                      stack_key: 0,
                      stack_key: 1,
                      stack_count: 0,
                      stack_count: 1,
                      dodge: 0,
                      dodge: 1,
                      effect_message: 0,
                      effect_message: 1,
                      dodgeable: 0,
                      dodgeable: 1,
                      parryable: 0,
                      parryable: 1,
                      properties: 1,
                      execute: 2,
                      magic_damage: 0,
                      magic_damage: 1,
                      attack_damage: 0,
                      attack_damage: 1,
                      melee_damage: 0,
                      melee_damage: 1,
                      magic_healing: 0,
                      magic_healing: 1,
                      cooldown: 0,
                      cooldown: 1,
                      stun: 0,
                      stun: 1,
                      increase_skills: 0,
                      increase_skills: 1,
                      help: 0,
                      script: 0,
                      script: 1]
    end
  end

end
