defmodule Systems.Race do
  use Systems.Reload

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload

      def keywords do
        __MODULE__.name
        |> String.split(~r/[ -]/)
        |> Enum.map(&(String.downcase(&1)))
      end
    end
  end

end
