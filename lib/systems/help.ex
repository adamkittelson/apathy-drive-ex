defmodule Systems.Help do
  use Systems.Reload

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      @after_compile Systems.Help

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
        |> String.replace("_", " ")
      end

    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      help = Help.find_by_module(__MODULE__)
      if help do
        Components.Keywords.value(help, __MODULE__.keywords)
        Components.Name.value(help, __MODULE__.name)
        Components.Help.value(help, __MODULE__.help)
      else
        {:ok, help} = Entity.init
        Entity.add_component(help, Components.Keywords, __MODULE__.keywords)
        Entity.add_component(help, Components.Name, __MODULE__.name)
        Entity.add_component(help, Components.Module, __MODULE__)
        Entity.add_component(help, Components.Help, __MODULE__.help)
        Help.add(help)
      end
    end
  end

end
