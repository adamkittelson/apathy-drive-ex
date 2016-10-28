defmodule ApathyDrive.RoomItem do
  use ApathyDrive.Web, :model

  schema "rooms_items" do
    belongs_to :room, ApathyDrive.Room
    belongs_to :item, ApathyDrive.Item
    field :level, :integer

    timestamps
  end

  @required_fields ~w(room_id item_id level)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> foreign_key_constraint(:room_id)
    |> foreign_key_constraint(:item_id)
  end

end
