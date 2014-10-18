defmodule Systems.Monster do
  use Systems.Reload
  import Systems.Text
  import Utility
  use Timex

  def spawn_monster(monster) do
    {:ok, entity} = Entity.init
    Entity.add_component(entity, Components.Name,        Components.Name.get_name(monster))
    Entity.add_component(entity, Components.Description, Components.Module.value(monster).properties[:description])
    Entity.add_component(entity, Components.Types, ["monster"])
    Entity.add_component(entity, Components.Limbs,  Components.Module.value(monster).properties[:limbs])
    Entity.add_component(entity, Components.Skills, %{})
    Entity.add_component(entity, Components.Effects, %{})
    Components.Skills.set_base_skills(entity, Components.Module.value(monster).properties[:skills])
    Entity.add_component(entity, Components.Stats,  Components.Module.value(monster).properties[:stats])
    Entity.add_component(entity, Components.HP, Systems.HP.max(entity))
    Entity.add_component(entity, Components.Mana, Systems.Mana.max(entity))
    Entity.add_component(entity, Components.Hunting, [])
    Entity.add_component(entity, Components.Combat, %{"break_at" => Date.convert(Date.now, :secs)})
    Entity.add_component(entity, Components.Module, Components.Module.value(monster))
    Entity.add_component(entity, Components.Attacks, %{})
    Entity.add_component(entity, Components.Abilities, [])
    Entity.add_component(entity, Components.Experience, 0)
    Entity.add_component(entity, Components.Level, 1)
    Entity.add_component(entity, Components.Items, [])
    Components.Attacks.reset_attacks(entity)
    Components.Abilities.reset_abilities(entity)
    Entity.add_to_type_collection(entity)
    entity
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    Components.Monsters.add_monster(room, monster)

    display_enter_message(room, monster)
  end

  def enter_message(entity) do
    default = "{{name}} enters from {{direction}}."
    if Entity.has_component?(entity, Components.Module) do
      Components.Module.value(entity).properties[:enter_message] || default
    else
      default
    end
  end

  def display_enter_message(room, monster) do
    message = monster
              |> enter_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => direction(room)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer,"scroll", "<p><span class='yellow'>#{message}</span></p>")
    end)
  end

  def display_enter_message(room, monster, direction) do
    message = monster
              |> enter_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => enter_direction(direction)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer,"scroll", "<p><span class='yellow'>#{message}</span></p>")
    end)
  end

  def display_exit_message(room, monster) do
    message = monster
              |> exit_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => ""
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer, "scroll", "<p><span class='yellow'>#{message}</span></p>")
    end)
  end

  def display_exit_message(room, monster, direction) do
    message = monster
              |> exit_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => exit_direction(direction)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer, "scroll", "<p><span class='yellow'>#{message}</span></p>")
    end)
  end

  def exit_message(entity) do
    default = "{{name}} exits {{direction}}."
    message = if Entity.has_component?(entity, Components.Module) do
      Components.Module.value(entity).properties[:exit_message] || default
    else
      default
    end
    String.replace(message, " .", ".")
  end

  def observers(room, monster) do
    spirits_in_room(room) ++ monsters_in_room(room, monster)
  end

  def monsters_in_room(room, monster) do
    room
    |> Components.Monsters.get_monsters
    |> Enum.reject(&(&1 == monster))
  end

  def spirits_in_room(room) do
    Characters.online
    |> Enum.filter(&(Parent.of(&1) == room))
    |> Enum.reject(&(!!Possession.possessed(&1)))
  end

  def exit_direction("up"),      do: "upwards"
  def exit_direction("down"),    do: "downwards"
  def exit_direction(direction), do: "to the #{direction}"

  def enter_direction("up"),      do: "above"
  def enter_direction("down"),    do: "below"
  def enter_direction(direction), do: "the #{direction}"

  def direction(room) do
    :random.seed(:os.timestamp)

    case Components.Exits.value(room) do
      nil ->
        "nowhere"
      {:error, :bad_module} ->
        "nowhere"
      {:error, :not_found} ->
        "nowhere"
      exits ->
        direction = exits
                    |> Map.keys
                    |> Enum.shuffle
                    |> List.first

        enter_direction(direction)
    end
  end

end
