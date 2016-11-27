defmodule ApathyDrive.Companion do
  alias ApathyDrive.{Character, Companion, Mobile}

  def hire_price(%Character{} = character) do
    character
    |> Mobile.power_at_level(character.level)
    |> div(10)
  end

  defimpl ApathyDrive.Mobile, for: Companion do

  end

end
