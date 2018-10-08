defmodule ApathyDrive.Regeneration do
  alias ApathyDrive.{Mobile, TimerManager}

  @ticks_per_round 5

  def tick_time(mobile) do
    round_length = Mobile.round_length_in_ms(mobile)
    round_length / @ticks_per_round
  end

  def duration_for_energy(mobile, energy) do
    round_length = Mobile.round_length_in_ms(mobile)
    max_energy = mobile.max_energy

    trunc(round_length * energy / max_energy)
  end

  def regenerate(mobile) do
    mobile
    |> regenerate_energy()
    |> regenerate_hp()
    |> regenerate_mana()
    |> schedule_next_tick()
    |> Mobile.update_prompt()
  end

  def energy_per_tick(mobile) do
    mobile.max_energy / @ticks_per_round
  end

  def regenerate_energy(mobile) do
    update_in(
      mobile,
      [Access.key!(:energy)],
      &min(mobile.max_energy, &1 + energy_per_tick(mobile))
    )
  end

  def regenerate_hp(%{hp: 1.0} = mobile), do: mobile

  def regenerate_hp(%{} = mobile) do
    hp = regen_per_tick(mobile, Mobile.hp_regen_per_round(mobile))

    mobile
    |> update_in([Access.key!(:hp)], &min(1.0, &1 + hp))
    |> Mobile.add_attribute_experience(%{health: 1})
  end

  def regenerate_mana(%{mana: 1.0} = mobile), do: mobile

  def regenerate_mana(%{} = mobile) do
    mana = regen_per_tick(mobile, Mobile.mana_regen_per_round(mobile))

    if attributes = Map.get(mobile, :mana_regen_attributes) do
      exp = Enum.reduce(attributes, %{}, &Map.put(&2, &1, 1))

      mobile
      |> update_in([Access.key!(:mana)], &min(1.0, &1 + mana))
      |> reset_mana_regen_attributes()
      |> Mobile.add_attribute_experience(exp)
    else
      mobile
      |> update_in([Access.key!(:mana)], &min(1.0, &1 + mana))
    end
  end

  def schedule_next_tick(mobile) do
    if mobile.max_energy > mobile.energy or mobile.mana < 1 or mobile.hp < 1 do
      TimerManager.send_after(
        mobile,
        {:energy_regen, tick_time(mobile), {:regenerate_energy, mobile.ref}}
      )
    else
      mobile
    end
  end

  defp reset_mana_regen_attributes(%{mana_regen_attributes: _, mana: 1.0} = mobile) do
    Map.put(mobile, :mana_regen_attributes, [])
  end

  defp reset_mana_regen_attributes(mobile), do: mobile

  def regen_per_tick(%{attack_target: nil} = _mobile, regen),
    do: regen / @ticks_per_round * 10

  def regen_per_tick(%{} = _mobile, regen), do: regen / @ticks_per_round
end
