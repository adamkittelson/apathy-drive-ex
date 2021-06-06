defmodule ApathyDrive.Socket do
  use ApathyDriveWeb, :model
  alias ApathyDrive.ItemInstance

  schema "sockets" do
    belongs_to(:item, ItemInstance)
    belongs_to(:socketed_item, ItemInstance)

    field(:number, :integer)
  end
end
