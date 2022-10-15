defmodule ApathyDrive.Stealth do
  alias ApathyDrive.Mobile

  def visible?(sneaker, observer, room) do
    stealth = Mobile.stealth(sneaker)

    perception = Mobile.perception(observer, room)

    stealth < perception
  end

  def invisible?(sneaker, observer, room) do
    !visible?(sneaker, observer, room)
  end

  def reveal(sneaker) do
    sneaker
    |> Map.put(:sneaking, false)
    |> Map.put(:resting, false)
  end
end
