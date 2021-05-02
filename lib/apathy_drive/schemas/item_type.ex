defmodule ApathyDrive.ItemType do
  use ApathyDriveWeb, :model

  schema "item_types" do
    field(:name, :string)
  end
end
