defmodule Systems.Ability do
  use Systems.Reload

  def abilities(entity) do
    Abilities.all
    |> Enum.filter fn(ability) ->
         Components.Module.value(ability).skill_prereqs_met?(entity)
       end
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
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

      def skill_prereqs_met?(entity) do
        skills
        |> Map.keys
        |> Enum.all? fn(skill) ->
             Systems.Skill.base(entity, skill) >= skills[skill]
           end
      end
    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
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
