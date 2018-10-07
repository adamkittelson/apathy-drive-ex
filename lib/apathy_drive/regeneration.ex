defmodule ApathyDrive.Regeneration do
  alias ApathyDrive.{Character, Mobile, TimerManager}

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
    |> update_bars
    |> Mobile.update_prompt()
  end

  def regenerate_energy(mobile) do
    amount_to_regenerate = div(mobile.max_energy, 100)

    update_in(mobile, [Access.key!(:energy)], &min(mobile.max_energy, &1 + amount_to_regenerate))
  end

  def regenerate_hp(%{attack_target: nil} = mobile) do
    hp = Mobile.hp_regen_per_round(mobile) / 10

    update_in(mobile, [Access.key!(:hp)], &min(1.0, &1 + hp))
  end

  def regenerate_hp(%{} = mobile) do
    hp = Mobile.hp_regen_per_round(mobile) / 100

    update_in(mobile, [Access.key!(:hp)], &min(1.0, &1 + hp))
  end

  def regenerate_mana(%{attack_target: nil} = mobile) do
    mana = Mobile.mana_regen_per_round(mobile) / 10

    update_in(mobile, [Access.key!(:mana)], &min(1.0, &1 + mana))
  end

  def regenerate_mana(%{} = mobile) do
    mana = Mobile.mana_regen_per_round(mobile) / 100

    update_in(mobile, [Access.key!(:mana)], &min(1.0, &1 + mana))
  end

  def schedule_next_tick(mobile) do
    if mobile.max_energy > mobile.energy or mobile.mana < 1 or mobile.hp < 1 do
      time_until_next_tick =
        mobile
        |> Mobile.round_length_in_ms()
        |> div(100)

      TimerManager.send_after(
        mobile,
        {:energy_regen, time_until_next_tick, {:regenerate_energy, mobile.ref}}
      )
    else
      mobile
    end
  end

  def update_bars(%Character{} = character) do
    Character.update_bars(character)
    character
  end

  def update_bars(%{} = mobile), do: mobile
end
