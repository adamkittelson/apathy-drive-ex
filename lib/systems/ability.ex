defmodule Systems.Ability do

  defmacro __using__(_opts) do
    quote do
      @after_compile Systems.Ability

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
      end
    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      {:ok, command} = Entity.init
      Entity.add_component(command, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(command, Components.Name, __MODULE__.name)
      Abilities.add(command)
    end
  end

end
