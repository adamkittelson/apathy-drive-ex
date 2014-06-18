defmodule Systems.Ability do
  use Systems.Reload

  def abilities(entity) do
    Abilities.all
    |> Enum.filter fn(ability) ->
         Components.Module.value(ability).useable_by?(entity)
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
