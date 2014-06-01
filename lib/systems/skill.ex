defmodule Systems.Skill do

  defmacro __using__(_opts) do
    quote do
      @after_compile Systems.Skill

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
      {:ok, skill} = Entity.init
      Entity.add_component(skill, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(skill, Components.Name, __MODULE__.name)
      Entity.add_component(skill, Components.PreReqs, __MODULE__.prereqs)
      Entity.add_component(skill, Components.Cost, __MODULE__.cost)
      Entity.add_component(skill, Components.Level, __MODULE__.level)
      Skills.add(__MODULE__.name, skill)
    end
  end

end
