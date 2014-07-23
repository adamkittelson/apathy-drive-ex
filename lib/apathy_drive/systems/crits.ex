defmodule Systems.Crits do
  use Systems.Reload
  import Systems.Text
  import Utility

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility

      def get_crit(nil), do: nil

      def get_crit(letter) do
        :random.seed(:os.timestamp)
        crits[letter]
        |> Enum.shuffle
        |> List.first
      end

      def roll_for_letter(crit_chance) do
        :random.seed(:os.timestamp)
        case :random.uniform(1_000_000) do
          roll when roll > crit_chance * 10_000 ->
            nil
          roll when roll > crit_chance * 5000 ->
            "A"
          roll when roll > crit_chance * 2500 ->
            "B"
          roll when roll > crit_chance * 1250 ->
            "C"
          roll when roll > crit_chance * 625 ->
            "D"
          _ ->
            "E"
        end
      end

      def random(crit_chance) do
        crit_chance
        |> roll_for_letter
        |> get_crit
      end

    end
  end

end