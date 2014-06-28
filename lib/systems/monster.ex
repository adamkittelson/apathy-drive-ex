defmodule Systems.Monster do
  use Systems.Reload

  def spawn_monster(monster) do
    {:ok, entity} = Entity.init
    Entity.add_component(entity, Components.Name,        Components.Name.get_name(monster))
    Entity.add_component(entity, Components.Description, Components.Module.value(monster).properties[:description])
    Entity.add_component(entity, Components.Types, ["monster"])
    Entity.add_to_type_collection(entity)
    entity
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    Entity.add_component(monster, Components.CurrentRoom, room)
    Components.Monsters.add_monster(room, monster)
  end

end
