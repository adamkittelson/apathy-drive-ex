defmodule ApathyDrive.CharacterItem do
  use ApathyDrive.Web, :model

  schema "characters_items" do
    belongs_to :character, ApathyDrive.Character
    belongs_to :item, ApathyDrive.Item
    field :level, :integer
    field :equipped, :boolean, default: false

    timestamps
  end

  @required_fields ~w(character_id item_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> foreign_key_constraint(:character_id)
    |> foreign_key_constraint(:room_id)
  end

end
