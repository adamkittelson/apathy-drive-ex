defmodule ApathyDrive.SocketableItemAffix do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Affix, Item, ItemType}

  schema "socketable_item_affixes" do
    belongs_to(:affix, Affix)
    belongs_to(:item, Item)
    belongs_to(:item_type, ItemType)
  end
end
