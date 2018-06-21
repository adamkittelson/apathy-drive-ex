defmodule ApathyDrive.AreaAlly do
  use ApathyDriveWeb, :model
  alias ApathyDrive.Area

  schema "areas_allies" do
    belongs_to(:area, Area)
    belongs_to(:ally, Area)
  end
end
