defmodule ApathyDrive.AffixItemType do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Affix, ItemType}

  schema "affixes_items_types" do
    belongs_to(:affix, Affix)
    belongs_to(:item_type, ItemType)

    field(:allowed, :boolean)
  end
end
