defmodule ApathyDrive.ItemRace do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Race, ItemRace, Item}

  schema "items_races" do
    field(:delete, :boolean, virtual: true)

    belongs_to(:race, Race)
    belongs_to(:item, Item)
  end

  @required_fields ~w(race_id)a

  def load_races(%Item{id: id} = item) do
    race_ids =
      __MODULE__
      |> where([ic], ic.item_id == ^id)
      |> select([ic], ic.race_id)
      |> Repo.all()

    Map.put(item, :required_races, race_ids)
  end

  def changeset(%ItemRace{} = rt, attrs) do
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
