defmodule Systems.Reload do

  defmacro __using__(_opts) do
    quote do

      def reload do
        Code.load_file(__MODULE__.module_info[:compile][:source] |> List.to_string)
      end

    end
  end

end
