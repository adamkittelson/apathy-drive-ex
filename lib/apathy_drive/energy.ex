defmodule ApathyDrive.Energy do
  alias ApathyDrive.Mobile

  def duration_for_energy(mobile, energy) do
    round_length = Mobile.round_length_in_ms(mobile)
    max_energy = mobile.max_energy

    trunc(round_length * energy / max_energy)
  end
end
