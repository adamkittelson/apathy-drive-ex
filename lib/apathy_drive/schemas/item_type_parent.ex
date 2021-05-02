defmodule ApathyDrive.ItemTypeParent do
  use ApathyDriveWeb, :model

  schema "item_type_parents" do
    belongs_to :item_type, ApathyDrive.ItemType
    belongs_to :parent, ApathyDrive.ItemType
  end
end
