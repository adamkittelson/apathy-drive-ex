defmodule Systems.Race do


  defmacro __using__(_opts) do
    quote do
    

      def keywords do
        __MODULE__.name
        |> String.split(~r/[ -]/)
        |> Enum.map(&(String.downcase(&1)))
      end
    end
  end

end
