defmodule ApathyDrive.ClassSpell do
  use ApathyDrive.Web, :model

  schema "classes_spells" do
    belongs_to :class, ApathyDrive.Class
    belongs_to :spell, ApathyDrive.Spell
    field :level, :integer

    timestamps
  end

  @required_fields ~w(class_id spell_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> foreign_key_constraint(:class_id)
    |> foreign_key_constraint(:spell_id)
  end

end
