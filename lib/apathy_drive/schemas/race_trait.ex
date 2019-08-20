defmodule ApathyDrive.RaceTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Race, RaceResistance, RaceTrait, Trait}

  schema "races_traits" do
    field(:value, ApathyDrive.JSONB)
    field(:delete, :boolean, virtual: true)

    belongs_to(:race, Race)
    belongs_to(:trait, Trait)
  end

  @required_fields ~w(trait_id value)a

  def load_traits(race_id) do
    __MODULE__
    |> where([mt], mt.race_id == ^race_id)
    |> preload([:trait])
    |> Repo.all()
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
      Map.put(abilities, trait.name, value)
    end)
    |> Trait.merge_traits(RaceResistance.load_resistances(race_id))
  end

  def changeset(%RaceTrait{} = rt, attrs) do
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
