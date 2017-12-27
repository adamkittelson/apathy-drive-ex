defmodule ApathyDrive.CritTrait do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Crit, CritResistance, Trait}

  schema "crits_traits" do
    field :value, ApathyDrive.JSONB

    belongs_to :crit, Crit
    belongs_to :trait, Trait
  end

  def load_traits(crit_id) do
    __MODULE__
    |> where([mt], mt.crit_id == ^crit_id)
    |> preload([:trait])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, crits ->
         Map.put(crits, trait.name, value)
       end)
    |> Map.merge(CritResistance.load_resistances(crit_id))
  end

  def add_trait_changeset(model, description) do
    model
    |> cast(%{description: description}, [:description])
    |> validate_required(:description)
    |> validate_length(:description, min: 20, max: 500)
  end

end
