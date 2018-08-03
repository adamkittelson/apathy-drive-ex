defmodule ApathyDrive.ShopItem do
  use ApathyDriveWeb, :model

  schema "shop_items" do
    field(:stock, :integer)
    field(:max_stock, :integer)
    field(:regen_frequency_in_minutes, :integer)
    field(:regen_chance, :integer)
    field(:regen_amount, :integer)
    field(:next_regen_at, :utc_datetime)

    belongs_to(:room, ApathyDrive.Room)
    belongs_to(:item, ApathyDrive.Item)
  end

  @required_fields ~w(room_id item_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> unique_constraint(:item_id, name: :shop_items_room_id_item_id_index)
    |> foreign_key_constraint(:item_id)
    |> foreign_key_constraint(:room_id)
  end

  def all do
    __MODULE__
    # |> Ecto.Query.where([si], si.room_id == ^id)
    |> Repo.all()
  end
end
