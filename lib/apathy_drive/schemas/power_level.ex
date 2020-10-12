defmodule ApathyDrive.PowerLevel do
  use ApathyDriveWeb, :model
  require Logger

  schema "power_levels" do
    field :name, :string
    field :hp_multiplier, :float
    field :damage_multiplier, :float
  end
end
