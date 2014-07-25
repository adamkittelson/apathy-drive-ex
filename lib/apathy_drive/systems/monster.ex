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
    Components.Skills.set_base_skills(entity, Components.Module.value(monster).properties[:skills])
    Entity.add_component(entity, Components.Stats,  Components.Module.value(monster).properties[:stats])
    Entity.add_component(entity, Components.HP, Systems.HP.max(entity))
    Entity.add_component(entity, Components.Mana, Systems.Mana.max(entity))
    Entity.add_component(entity, Components.Hunting, [])
    Entity.add_component(entity, Components.Combat, %{"break_at" => Date.convert(Date.now, :secs)})
    Entity.add_component(entity, Components.Module, Components.Module.value(monster))
    Entity.add_component(entity, Components.Attacks, %{})
    Components.Attacks.reset_attacks(entity)
    Entity.add_to_type_collection(entity)
    entity
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    Components.Monsters.add_monster(room, monster)

    message = enter_message(monster)
    opts = %{
      "name" => Components.Name.value(monster),
      "direction" => direction(room)
    }

    Systems.Room.characters_in_room(room)
    |> Enum.each(fn(character) ->
      send_message(character, "scroll", "<p><span class='yellow'>#{interpolate(message, opts)}</span></p>")
    end)
  end

  def enter_message(entity) do
    default = "{{name}} enters from {{direction}}."
    if Entity.has_component?(entity, Components.Module) do
      Components.Module.value(entity).properties[:enter_message] || default
    else
      default
    end
  end

  def direction(room) do
    :random.seed(:os.timestamp)

    case Components.Exits.value(room) do
      nil ->
        "nowhere"
      {:error, :bad_module} ->
        "nowhere"
      exits ->
        direction = exits
                    |> Map.keys
                    |> Enum.shuffle
                    |> List.first

        case direction do
          "up" ->
            "above"
          "down" ->
            "below"
           direction ->
             "the #{direction}"
        end
    end
  end

end
