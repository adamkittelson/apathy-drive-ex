defmodule ApathyDrive.SpellAbility do
  use ApathyDrive.Web, :model

  schema "spells_abilities" do
    belongs_to :train, ApathyDrive.Trait
    belongs_to :ability, ApathyDrive.Ability
    field :value, ApathyDrive.JSONB

    timestamps()
  end

  @required_fields ~w(ability_id spell_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> foreign_key_constraint(:ability_id)
    |> foreign_key_constraint(:trait_id)
  end

end
