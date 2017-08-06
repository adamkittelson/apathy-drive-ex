defmodule ApathyDrive.ItemAbility do
  use ApathyDrive.Web, :model

  schema "items_abilities" do
    belongs_to :ability, ApathyDrive.Ability
    belongs_to :item, ApathyDrive.Item
    field :value, ApathyDrive.JSONB

    timestamps()
  end

  @required_fields ~w(ability_id item_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> foreign_key_constraint(:ability_id)
    |> foreign_key_constraint(:item_id)
  end

end
