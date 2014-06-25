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
    if ability.properties[:target] == "self" do
      execute(ability, entity, Components.Name.value(entity))
    else
      Components.Player.send_message(entity, ["scroll", "<p><span class='cyan'>You must supply a target.</span></p>"])
    end
  end

  def execute(ability, entity, target) do
    if ability.properties[:casting_time] do
      delay_execution(ability, entity, target)
    else
      execute(ability, entity, target, :verify_target)
    end
  end

  def execute(ability, entity, target, :verify_target) do
    room = Components.CurrentRoom.get_current_room(entity)
    target_entity = find_target(ability, room, target)
    if target_entity do
      execute(ability, entity, target_entity, :mana)
    else
      Components.Player.send_message(entity, ["scroll", "<p><span class='cyan'>Can't find #{target} here!  Your spell fails.</span></p>"])
    end
  end

  def execute(ability, entity, target, :mana) do
    if ability.properties[:mana_cost] do
      if Components.Mana.subtract(entity, ability.properties[:mana_cost]) do
        Systems.Prompt.update(entity)
        execute(ability, entity, target, :execute)
      else
        Components.Player.send_message(entity, ["scroll", "<p><span class='dark-cyan'>You don't have enough mana.</span></p>"])
      end
    else
      execute(ability, entity, target, :execute)
    end
  end

  def execute(ability, entity, target, :execute) do
    display_cast_message(ability, entity, target)
    Systems.Damage.do_damage(ability, entity, target)
  end

  def display_cast_message(ability, entity, target) do
    Components.CurrentRoom.get_current_room(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->
      cond do
        character == entity ->
          Components.Player.send_message(character, ["scroll", interpolate(ability.properties[:user_message], entity, target)])
        character == target ->
          Components.Player.send_message(character, ["scroll", interpolate(ability.properties[:target_message], entity, target)])
        true ->
          Components.Player.send_message(character, ["scroll", interpolate(ability.properties[:observer_message], entity, target)])
      end
    end)
  end

  def kill(entity, target) do
    Components.CurrentRoom.get_current_room(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->
      cond do
        character == entity ->
          Components.Player.send_message(character, ["scroll", interpolate("<p>You just killed {{target}}!</p>", entity, target)])
        character == target ->
          Components.Player.send_message(character, ["scroll", interpolate("<p>{{user}} just killed you!</p>", entity, target)])
        true ->
          Components.Player.send_message(character, ["scroll", interpolate("<p>{{user}} just killed {{target}}!</p>", entity, target)])
      end
    end)
  end

  def delay_execution(ability, entity, target) do
    display_precast_message(ability, entity)

    delay(ability.properties[:casting_time]) do
      execute(ability, entity, target, :verify_target)
    end
  end

  def display_precast_message(ability, entity) do
    Components.CurrentRoom.get_current_room(entity)
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->
         if character == entity do
           Components.Player.send_message(entity, ["scroll", "<p><span class='cyan'>You begin your casting.</span></p>"])
         else
           Components.Player.send_message(entity, ["scroll", "<p><span class='cyan'>#{Components.Name.value(entity)} begins casting a spell.</span></p>"])
         end
       end)
  end

  def find_target(ability, room, target) do
    case ability.properties[:target] do
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

      @after_compile Systems.Ability

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

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      ability = Abilities.find_by_module(__MODULE__)
      if ability do
        Components.Keywords.value(ability, __MODULE__.keywords)
        Components.Name.value(ability, __MODULE__.name)
        Components.Help.value(ability, __MODULE__.help)
      else
        {:ok, ability} = Entity.init
        Entity.add_component(ability, Components.Keywords, __MODULE__.keywords)
        Entity.add_component(ability, Components.Name, __MODULE__.name)
        Entity.add_component(ability, Components.Module, __MODULE__)
        Entity.add_component(ability, Components.Help, __MODULE__.help)
        Abilities.add(__MODULE__.name, ability)
        Help.add(ability)
      end
    end
  end

end
