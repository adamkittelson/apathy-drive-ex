defmodule ApathyDrive.CharacterAbility do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, Character}

  schema "characters_abilities" do
    belongs_to(:ability, Ability)
    belongs_to(:character, Character)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(ability_id class_id)a)
  end
end
