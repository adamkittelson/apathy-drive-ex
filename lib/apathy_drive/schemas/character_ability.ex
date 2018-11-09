defmodule ApathyDrive.CharacterAbility do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, Character}

  schema "characters_abilities" do
    field(:passive, :boolean)

    belongs_to(:ability, Ability)
    belongs_to(:character, Character)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(ability_id class_id passive)a)
  end

  def abilities_at_level(class_id, level) do
    ApathyDrive.ClassAbility
    |> Ecto.Query.where([ss], ss.class_id == ^class_id and ss.level <= ^level)
    |> Ecto.Query.preload([:ability])
    |> Repo.all()
  end
end
