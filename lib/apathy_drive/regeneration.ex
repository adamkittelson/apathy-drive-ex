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

    max(0, trunc(round_length * energy / max_energy))
  end

  def regenerate(mobile) do
    mobile
    |> regenerate_energy()
    |> regenerate_hp()
    |> regenerate_mana()
    |> schedule_next_tick()
    |> Map.put(:last_tick_at, DateTime.utc_now())
    |> Mobile.update_prompt()
  end

  def energy_per_tick(mobile) do
    mobile.max_energy / @ticks_per_round
  end

  def energy_since_last_tick(%{last_tick_at: nil} = mobile), do: energy_per_tick(mobile)

  def energy_since_last_tick(%{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :milliseconds)
    energy_per_tick = energy_per_tick(mobile)
    energy = energy_per_tick * ms_since_last_tick / tick_time(mobile)

    min(energy, energy_per_tick)
  end

  def hp_since_last_tick(%{last_tick_at: nil} = mobile),
    do: regen_per_tick(mobile, Mobile.hp_regen_per_round(mobile))

  def hp_since_last_tick(%{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :milliseconds)
    hp_per_tick = regen_per_tick(mobile, Mobile.hp_regen_per_round(mobile))
    hp = hp_per_tick * ms_since_last_tick / tick_time(mobile)

    min(hp, hp_per_tick)
  end

  def mana_since_last_tick(%{last_tick_at: nil} = mobile),
    do: regen_per_tick(mobile, Mobile.mana_regen_per_round(mobile))

  def mana_since_last_tick(%{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :milliseconds)
    mana_per_tick = regen_per_tick(mobile, Mobile.mana_regen_per_round(mobile))
    mana = mana_per_tick * ms_since_last_tick / tick_time(mobile)

    min(mana, mana_per_tick)
  end

  def regenerate_energy(mobile) do
    energy = energy_since_last_tick(mobile)

    update_in(
      mobile,
      [Access.key!(:energy)],
      &min(mobile.max_energy, &1 + energy)
    )
  end

  def regenerate_hp(%{hp: 1.0} = mobile), do: mobile

  def regenerate_hp(%{} = mobile) do
    hp = hp_since_last_tick(mobile)

    mobile
    |> update_in([Access.key!(:hp)], &min(1.0, &1 + hp))
  end

  def regenerate_mana(%{mana: 1.0} = mobile), do: mobile

  def regenerate_mana(%{} = mobile) do
    mana = mana_since_last_tick(mobile)

    if Map.get(mobile, :mana_regen_attributes) do
      mobile
      |> update_in([Access.key!(:mana)], &min(1.0, &1 + mana))
      |> reset_mana_regen_attributes()
    else
      mobile
      |> update_in([Access.key!(:mana)], &min(1.0, &1 + mana))
    end
  end

  def schedule_next_tick(mobile) do
    TimerManager.send_after(mobile, {:heartbeat, tick_time(mobile), {:heartbeat, mobile.ref}})
  end

  defp reset_mana_regen_attributes(%{mana_regen_attributes: _, mana: 1.0} = mobile) do
    Map.put(mobile, :mana_regen_attributes, [])
  end

  defp reset_mana_regen_attributes(mobile), do: mobile

  def regen_per_tick(%{} = mobile, regen) do
    if TimerManager.time_remaining(mobile, :rest_timer) > 0 do
      regen / @ticks_per_round
    else
      regen / @ticks_per_round * 10
    end
  end
end
