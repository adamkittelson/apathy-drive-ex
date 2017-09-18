defmodule ApathyDrive.RoomItem do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Item

  schema "rooms_items" do
    field :level, :integer
    field :hidden, :boolean

    belongs_to :item, ApathyDrive.Item
    belongs_to :room, ApathyDrive.Room
  end

  def load_items(room_id) do
    __MODULE__
    |> where([ri], ri.room_id == ^room_id)
    |> preload(:item)
    |> Repo.all
    |> Enum.map(&Item.from_assoc/1)
  end

end
