defmodule ApathyDrive.SpellAbility do
  use ApathyDrive.Web, :model

  schema "spells_abilities" do
    belongs_to :ability, ApathyDrive.Ability
    belongs_to :spell, ApathyDrive.Spell
    field :value, ApathyDrive.JSONB

    timestamps()
  end

  @required_fields ~w(ability_id spell_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> foreign_key_constraint(:ability_id)
    |> foreign_key_constraint(:spell_id)
  end

end
