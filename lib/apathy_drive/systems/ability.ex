defmodule Systems.Ability do
  use Systems.Reload
  import Systems.Text
  import Utility
  import Timer, except: [start: 0]

  def abilities(entity) do
    Abilities.all
    |> Enum.filter fn(ability) ->
         Components.Module.value(ability).useable_by?(entity)
       end
  end

  def execute(ability, entity, nil) do
    if ability[:target] == "self" do
      execute(ability, entity, Components.Name.value(entity))
    else
      send_message(entity, "scroll", "<p><span class='cyan'>You must supply a target.</span></p>")
    end
  end

  def execute(ability, entity, target) do
    if ability[:casting_time] do
      delay_execution(ability, entity, target)
    else
      execute(ability, entity, target, :verify_target)
    end
  end

  def execute(ability, entity, target, :verify_target) do
    room = Parent.of(entity)
    target_entity = find_target(ability, room, entity, target)
    if target_entity do
      execute(ability, entity, target_entity, :mana)
    else
      send_message(entity, "scroll", "<p><span class='cyan'>Can't find #{target} here!  Your spell fails.</span></p>")
    end
  end

  def execute(ability, entity, target, :mana) do
    if ability[:mana_cost] do
      if Components.Mana.subtract(entity, ability[:mana_cost]) do
        ManaRegen.add(entity)
        Systems.Prompt.update(entity)
        execute(ability, entity, target, :dodge)
      else
        send_message(entity, "scroll", "<p><span class='dark-cyan'>You don't have enough mana.</span></p>")
      end
    else
      execute(ability, entity, target, :dodge)
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
        execute(ability, entity, target, :execute)
      end
    else
      execute(ability, entity, target, :execute)
    end
  end

  def execute(ability, entity, target, :execute) do
    {limb, damage, crit} = Systems.Damage.calculate_damage(ability, target)
    display_cast_message(ability, entity, target)
    display_crit_message(crit, entity, target)
    Systems.Crits.add_crit_effects(damage, target, crit[:effects])
    if damage && damage > 0 do
      damage_limb(target, limb, damage)
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
      cond do
        character == entity ->
          send_message(character, "scroll", "<p><span class='red'>#{interpolate(crit[:user_message], opts) |> capitalize_first}</span></p>")
        character == target ->
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
      cond do
        character == entity ->
          send_message(character, "scroll", interpolate(ability[:user_message], opts))
        character == target ->
          send_message(character, "scroll", interpolate(ability[:target_message], opts))
        true ->
          send_message(character, "scroll", interpolate(ability[:spectator_message], opts))
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
      cond do
        character == entity ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{user_dodge_message}</span></p>")
        character == target ->
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
      cond do
        character == entity ->
          send_message(character, "scroll", "<p><span class='dark-cyan'>#{user_parry_message}</span></p>")
        character == target ->
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
      cond do
        character == entity ->
          send_message(character, "scroll", interpolate("<p>You just killed {{target}}!</p>", opts))
        character == target ->
          send_message(character, "scroll", interpolate("<p>{{user}} just killed you!</p>", opts))
        true ->
          send_message(character, "scroll", interpolate("<p>{{user}} just killed {{target}}!</p>", opts))
      end
    end)
  end

  def delay_execution(ability, entity, target) do
    display_precast_message(ability, entity)

    apply_after(ability[:casting_time] |> seconds, ability[:env]) do
      execute(ability, entity, target, :verify_target)
    end
  end

  def display_precast_message(_ability, entity) do
    Parent.of(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->
         if character == entity do
           send_message(entity, "scroll", "<p><span class='dark-cyan'>You begin your casting.</span></p>")
         else
           send_message(entity, "scroll", "<p><span class='dark-cyan'>#{Components.Name.value(entity)} begins casting a spell.</span></p>")
         end
       end)
  end

  def find_target(ability, room, _entity, target) do
    case ability[:target] do
      "character" ->
        room
        |> Systems.Room.characters_in_room
        |> Systems.Match.first(:name_contains, target)
      "living" ->
        room
        |> Systems.Room.living_in_room
        |> Systems.Match.first(:name_contains, target)
      _ ->
        nil
    end
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import Timer, except: [start: 0]

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
        |> String.replace("_", " ")
      end

      def keywords do
        name |> String.split
      end
    end
  end

end
