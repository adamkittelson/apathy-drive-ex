defmodule Systems.Help do
  use Systems.Reload

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload

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
