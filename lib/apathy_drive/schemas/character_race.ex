defmodule ApathyDrive.CharacterRace do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, CharacterRace, Race}

  schema "characters_races" do
    field(:strength_experience, :integer, default: 0)
    field(:agility_experience, :integer, default: 0)
    field(:intellect_experience, :integer, default: 0)
    field(:willpower_experience, :integer, default: 0)
    field(:health_experience, :integer, default: 0)
    field(:charm_experience, :integer, default: 0)
    field(:delete, :boolean, virtual: true)

    belongs_to(:character, Character)
    belongs_to(:race, Race)
  end

  @required_fields ~w(character_id race_id)a

  def changeset(%CharacterRace{} = rt, attrs) do
    rt
    |> cast(attrs, [:delete | @required_fields])
    |> validate_required(@required_fields)
    |> mark_for_deletion()
  end

  defp mark_for_deletion(changeset) do
    # If delete was set and it is true, let's change the action
    if get_change(changeset, :delete) do
      %{changeset | action: :delete}
    else
      changeset
    end
  end
end
