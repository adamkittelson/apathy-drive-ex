defmodule ApathyDrive.AreaAlly do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Area

  schema "areas_allies" do
    belongs_to :area, Area
    belongs_to :ally, Area
  end

end
