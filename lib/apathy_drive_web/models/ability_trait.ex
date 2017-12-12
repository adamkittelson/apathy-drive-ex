defmodule ApathyDrive.AbilityTrait do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, AbilityResistance, Trait}

  schema "abilities_traits" do
    field :value, ApathyDrive.JSONB

    belongs_to :ability, Ability
    belongs_to :trait, Trait
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(trait_id value)a)
  end

  def load_traits(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:trait])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
         Map.put(abilities, trait.name, value)
       end)
    |> Map.merge(AbilityResistance.load_resistances(ability_id))
  end

  def add_trait_changeset(model, description) do
    model
    |> cast(%{description: description}, [:description])
    |> validate_required(:description)
    |> validate_length(:description, min: 20, max: 500)
  end

end
