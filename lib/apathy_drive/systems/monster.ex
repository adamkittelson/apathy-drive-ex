defmodule Systems.Monster do
  use Systems.Reload

  def spawn_monster(monster) do
    {:ok, entity} = Entity.init
    Entity.add_component(entity, Components.Name,        Components.Name.get_name(monster))
    Entity.add_component(entity, Components.Description, Components.Module.value(monster).properties[:description])
    Entity.add_component(entity, Components.Types, ["monster"])
    Entity.add_component(entity, Components.Limbs,  Components.Module.value(monster).properties[:limbs])
    Entity.add_component(entity, Components.Skills, Components.Module.value(monster).properties[:skills] || %{})
    Entity.add_component(entity, Components.Stats,  Components.Module.value(monster).properties[:stats])
    Entity.add_component(entity, Components.HP, Systems.HP.max_hp(entity))
    Entity.add_component(entity, Components.Mana, Systems.Mana.max_mana(entity))
    Entity.add_component(entity, Components.Module, Components.Module.value(monster))
    Entity.add_to_type_collection(entity)
    entity
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    Entity.add_component(monster, Components.CurrentRoom, Components.ID.value(room))
    Components.Monsters.add_monster(room, monster)
  end

end
