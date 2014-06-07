defmodule Systems.Skill do
  use Systems.Reload

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
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

      def base(entity) do
        Systems.Trainer.rating(__MODULE__, entity)
      end

    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      {:ok, skill} = Entity.init
      Entity.add_component(skill, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(skill, Components.Name, __MODULE__.name)
      Entity.add_component(skill, Components.Module, __MODULE__)
      Skills.add(__MODULE__.name, skill)
    end
  end

end
