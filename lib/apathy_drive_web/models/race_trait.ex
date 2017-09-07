defmodule ApathyDrive.RaceTrait do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Race, Trait}

  schema "races_traits" do
    field :value, ApathyDrive.JSONB

    belongs_to :race, Race
    belongs_to :trait, Trait
  end

  def load_traits(race_id) do
    __MODULE__
    |> where([mt], mt.race_id == ^race_id)
    |> preload([:trait])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
         Map.put(abilities, trait.name, value)
       end)
  end

end
