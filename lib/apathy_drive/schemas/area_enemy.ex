defmodule ApathyDrive.AreaEnemy do
  use ApathyDriveWeb, :model
  alias ApathyDrive.Area

  schema "areas_enemies" do
    belongs_to(:area, Area)
    belongs_to(:enemy, Area)
  end
end
