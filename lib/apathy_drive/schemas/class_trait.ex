defmodule ApathyDrive.ClassTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Class, ClassTrait, Trait}

  schema "classes_traits" do
    field(:value, ApathyDrive.JSONB)
    field(:delete, :boolean, virtual: true)

    belongs_to(:class, Class)
    belongs_to(:trait, Trait)
  end

  @required_fields ~w(trait_id value)a

  def load_traits(class_id) do
    __MODULE__
    |> where([mt], mt.class_id == ^class_id)
    |> preload([:trait])
    |> Repo.all()
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
      Map.put(abilities, trait.name, value)
    end)
  end

  def changeset(%ClassTrait{} = rt, attrs) do
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
