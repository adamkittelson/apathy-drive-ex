defmodule ApathyDrive.Exit do

  def direction_description(direction) do
    case direction do
    "up" ->
      "above you"
    "down" ->
      "below you"
    direction ->
      "to the #{direction}"
    end
  end

end
