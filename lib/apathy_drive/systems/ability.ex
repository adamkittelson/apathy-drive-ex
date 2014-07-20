defmodule Systems.Ability do
  use Systems.Reload
  import Systems.Text
  import Utility

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
        execute(ability, entity, target, :execute)
      else
        send_message(entity, "scroll", "<p><span class='dark-cyan'>You don't have enough mana.</span></p>")
      end
    else
      execute(ability, entity, target, :execute)
    end
  end

  def execute(ability, entity, target, :execute) do
    {limb, damage} = Systems.Damage.calculate_damage(ability, entity, target)
    display_cast_message(ability, entity, target, %{"damage" => damage})
    Systems.Damage.do_damage(target, limb, damage)
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
          send_message(character, "scroll", interpolate(ability[:observer_message], opts))
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

    delay(ability[:casting_time], ability.name) do
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
