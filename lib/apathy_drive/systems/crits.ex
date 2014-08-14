defmodule Systems.Crits do
  use Systems.Reload
  alias Systems.Effect
  use Timex

  def add_crit_effects(damage, target, effects) do
    add_stat_mod_effects(target, effects[:stat_mod])
    add_skill_mod_effects(target, effects[:skill_mod])
    add_stun_effect(target, effects[:stun])
  end

  def add_stun_effect(target, nil), do: nil
  def add_stun_effect(target, duration) do
    Effect.add(target, :stunned, duration)
  end

  def add_stat_mod_effects(target, nil), do: nil
  def add_stat_mod_effects(target, []),  do: nil
  def add_stat_mod_effects(target, stat_mods) do
    Enum.each(stat_mods, fn(stat_mod) ->
      Effect.add(target, Map.put(%{}, stat_mod[:stat], stat_mod[:amount]), stat_mod[:duration])
    end)
  end

  def add_skill_mod_effects(target, nil), do: nil
  def add_skill_mod_effects(target, []),  do: nil
  def add_skill_mod_effects(target, skill_mods) do
    Enum.each(skill_mods, fn(skill_mod) ->
      Effect.add(target, Map.put(%{}, skill_mod[:skill], skill_mod[:amount]), skill_mod[:duration])
    end)
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import Timer, except: [start: 0]

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
            "A"
          roll when roll > crit_chance * 5000 ->
            "B"
          roll when roll > crit_chance * 2500 ->
            "C"
          roll when roll > crit_chance * 1250 ->
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