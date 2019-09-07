defmodule ApathyDrive.Stealth do
  alias ApathyDrive.Mobile

  def visible?(sneaker, observer, room) do
    stealth = Mobile.stealth_at_level(sneaker, sneaker.level)

    perception = Mobile.perception_at_level(observer, observer.level, room)

    stealth < perception
  end

  def invisible?(sneaker, observer, room) do
    !visible?(sneaker, observer, room)
  end

  def reveal(sneaker) do
    sneaker
    |> Map.put(:sneaking, false)
  end
end
