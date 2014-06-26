defmodule Systems.Monster do
  use Systems.Reload

  def spawn_monster(monster) do
    {:ok, entity} = Entity.init
    Entity.add_component(entity, Components.Name,        Components.Name.get_name(monster))
    Entity.add_component(entity, Components.Description, Components.Description.get_description(monster))
    Entity.add_component(entity, Components.Types, ["monster"])
    Entity.add_to_type_collection(entity)
    entity
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    Entity.add_component(monster, Components.CurrentRoom, room)
    Components.Monsters.add_monster(room, monster)
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility

      @after_compile Systems.Monster

    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      ability = MonsterTemplates.find_by_module(__MODULE__)
      if ability do
        Components.Keywords.value(ability, __MODULE__.keywords)
        Components.Name.value(ability, __MODULE__.name)
      else
        {:ok, mt} = Entity.init
        Entity.add_component(mt, Components.Keywords, __MODULE__.keywords)
        Entity.add_component(mt, Components.Name, __MODULE__.name)
        Entity.add_component(mt, Components.Module, __MODULE__)

        id = __MODULE__.module_info[:compile][:source]
             |> List.to_string
             |> String.replace(File.cwd!, "")

        MonsterTemplates.add(id, mt)
      end
    end
  end

end
