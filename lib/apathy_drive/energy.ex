defmodule ApathyDrive.Energy do
  alias ApathyDrive.{Character, Mobile, TimerManager}

  def duration_for_energy(mobile, energy) do
    round_length = Mobile.round_length_in_ms(mobile)
    max_energy = mobile.max_energy

    trunc(round_length * energy / max_energy)
  end

  def regenerate(mobile) do
    amount_to_regenerate = div(mobile.max_energy, 100)

    mobile
    |> update_in([Access.key!(:energy)], &min(mobile.max_energy, &1 + amount_to_regenerate))
    |> schedule_next_tick()
    |> update_energy_bar
  end

  def schedule_next_tick(mobile) do
    if mobile.max_energy > mobile.energy do
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

  def update_energy_bar(%Character{} = character) do
    Character.update_energy(character)
    character
  end

  def update_energy_bar(%{} = mobile), do: mobile
end
