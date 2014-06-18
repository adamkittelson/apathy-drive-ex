defmodule Systems.Race do
  use Systems.Reload

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      @after_compile Systems.Race

      def keywords do
        __MODULE__.name
        |> String.split(~r/[ -]/)
        |> Enum.map(&(String.downcase(&1)))
      end
    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      race = Races.find_by_module(__MODULE__)
      if race do
        Components.Name.value(race, __MODULE__.name)
        Components.Help.value(race, __MODULE__.help)
        Components.Limbs.value(race, __MODULE__.limbs)
        Components.Stats.value(race, __MODULE__.stats)
      else
        {:ok, race} = Entity.init
        Entity.add_component(race, Components.Keywords, __MODULE__.keywords)
        Entity.add_component(race, Components.Name, __MODULE__.name)
        Entity.add_component(race, Components.Help, __MODULE__.help)
        Entity.add_component(race, Components.Limbs, __MODULE__.limbs)
        Entity.add_component(race, Components.Stats, __MODULE__.stats)
        Entity.add_component(race, Components.Module, __MODULE__)
        Races.add(race)
        Help.add(race)
      end

    end
  end

end
