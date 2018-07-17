defmodule ApathyDrive.ItemInstance do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Item, Room}

  schema "items_instances" do
    field(:level, :integer)
    field(:equipped, :boolean)
    field(:hidden, :boolean)
    field(:purchased, :boolean, default: false)

    belongs_to(:item, Item)
    belongs_to(:room, Room)
    belongs_to(:character, Character)
  end

  def load_items(%Room{id: id}) do
    __MODULE__
    |> where([ri], ri.room_id == ^id)
    |> preload(:item)
    |> Repo.all()
    |> Enum.map(&Item.from_assoc/1)
  end

  def load_items(%Character{id: id}) do
    __MODULE__
    |> where([ri], ri.character_id == ^id)
    |> preload(:item)
    |> Repo.all()
    |> Enum.map(&Item.from_assoc/1)
  end
end
