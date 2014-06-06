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
      {:ok, race} = Entity.init
      Entity.add_component(race, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(race, Components.Name, __MODULE__.name)
      Entity.add_component(race, Components.Help, __MODULE__.help)
      Entity.add_component(race, Components.Limbs, __MODULE__.limbs)
      Entity.add_component(race, Components.Stats, __MODULE__.stats)
      Races.add(race)
      Help.add(race)
    end
  end

end
