defmodule ApathyDrive.Title do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Class, Text, Title}

  schema "titles" do
    field(:level, :integer)
    field(:text, :string)
    field(:delete, :boolean, virtual: true)

    belongs_to(:class, Class)
  end

  @required_fields ~w(class_id level text)a

  def for_character(%Character{class_id: id, level: level} = character) do
    __MODULE__
    |> where([t], t.class_id == ^id and t.level <= ^level)
    |> order_by(desc: :level)
    |> limit(1)
    |> select([t], t.text)
    |> Repo.one()
    |> Text.interpolate(%{"user" => character})
  end

  def changeset(%Title{} = t, attrs) do
    t
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
