defmodule Systems.Monster do

  def spawn_monster(monster) do
    {:ok, entity} = ApathyDrive.Entity.init
    ApathyDrive.Entity.add_component(entity, Components.Name,        Components.Name.get_name(monster))
    ApathyDrive.Entity.add_component(entity, Components.Description, Components.Description.get_description(monster))
    entity
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    ApathyDrive.Entity.add_component(monster, Components.CurrentRoom, room)
    Components.Monsters.add_monster(room, monster)
  end
end
