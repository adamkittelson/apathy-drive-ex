defmodule ApathyDrive.ShopItemType do
  use ApathyDriveWeb, :model

  schema "shop_item_types" do
    field :amount, :integer
    field :quality, :string

    belongs_to :shop, ApathyDrive.Shop
    belongs_to :item_type, ApathyDrive.ItemType
  end
end
