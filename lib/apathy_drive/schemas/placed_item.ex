defmodule ApathyDrive.PlacedItem do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Item, Room}

  schema "rooms_placed_items" do
    belongs_to(:room, Room)
    belongs_to(:item, Item)
    field(:hidden, :boolean)
  end
end
