defmodule Systems.Help do


  defmacro __using__(_opts) do
    quote do
    

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

end
