defmodule ApathyDrive.CharacterClass do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, CharacterClass, Class}

  schema "characters_classes" do
    field(:experience, :float, default: 0.0)
    field(:level, :integer)
    field(:exp_buffer, :integer)
    field(:delete, :boolean, virtual: true)

    belongs_to(:character, Character)
    belongs_to(:class, Class)
  end

  @required_fields ~w(character_id class_id)a

  def changeset(%CharacterClass{} = rt, attrs) do
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
