defmodule ApathyDrive.ItemInstance do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Class, Item, Room, Shop}

  schema "items_instances" do
    field(:level, :integer)
    field(:equipped, :boolean)
    field(:hidden, :boolean)
    field(:purchased, :boolean, default: false)
    field(:owner_id, :integer)
    field(:delete_at, :utc_datetime_usec)
    field(:uses, :integer)
    field(:getable, :boolean)

    # overrides name and description of an item
    # used for severed limbs
    field(:name, :string)
    field(:description, :string)
    field(:room_destruct_message, :string)

    belongs_to(:item, Item)
    belongs_to(:room, Room)
    belongs_to(:beacon_room, Room)
    belongs_to(:character, Character)
    belongs_to(:class, Class)
    belongs_to(:shop, Shop)
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
