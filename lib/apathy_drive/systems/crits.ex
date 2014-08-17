defmodule Systems.Crits do
  use Systems.Reload
  alias Systems.Effect
  use Timex
  import BlockTimer
  import Utility

  def add_crit_effects(damage, target, effects) do
    add_damage_over_time_effect(target, damage, effects[:damage_over_time])
    add_stat_mod_effects(target, effects[:stat_mod])
    add_skill_mod_effects(target, effects[:skill_mod])
    add_stun_effect(target, effects[:stun])
    apply_limb_loss_effects(target, effects[:limb_loss])
  end

  def add_damage_over_time_effect(_target, _damage, nil), do: nil
  def add_damage_over_time_effect(target, damage, damage_over_time) do
    {:ok, timer} = apply_interval 1 |> seconds do
      send_message(target, "scroll", "<p>You take damage from your wounds!</p>")
      Systems.Damage.do_damage(target, damage * damage_over_time[:damage])
    end
    Effect.add(target, %{:timers => [timer]}, damage_over_time[:duration])
  end

  def apply_limb_loss_effects(target, nil), do: nil
  def apply_limb_loss_effects(target, []),  do: nil
  def apply_limb_loss_effects(target, limb_loss) do
    Enum.each(limb_loss, fn(limb_loss) ->
      get_limb(target, limb_loss)
      |> apply_limb_loss_effect(target, limb_loss[:kind])
    end)
  end

  def apply_limb_loss_effect(nil, _target, _kind), do: nil
  def apply_limb_loss_effect(limb_name, target, "cripple") do
    Systems.Limbs.cripple_limb(target, limb_name)
  end

  def apply_limb_loss_effect(limb_name, target, "sever") do
    Systems.Limbs.sever_limb(target, limb_name)
  end

  def get_limb(target, %{:kind => "cripple"} = limb_loss) do
    random_limb(Components.Limbs.uncrippled_limbs(target, limb_loss[:limb]))
  end

  def get_limb(target, %{:kind => "sever"} = limb_loss) do
    random_limb(Components.Limbs.unsevered_limbs(target, limb_loss[:limb]))
  end

  def random_limb([]), do: nil
  def random_limb(limb_names) do
    :random.seed(:os.timestamp)
    limb_names
    |> Enum.shuffle
    |> List.first
  end

  def add_stun_effect(target, nil), do: nil
  def add_stun_effect(target, duration) do
    Effect.add(target, %{:stunned => true}, duration)
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
      import BlockTimer

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