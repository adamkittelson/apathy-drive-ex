defmodule ApathyDrive.AbilityTrait do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, Trait}

  schema "abilities_traits" do
    field :value, ApathyDrive.JSONB

    belongs_to :ability, Ability
    belongs_to :trait, Trait
  end

  def load_traits(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:trait])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
         Map.put(abilities, trait.name, value)
       end)
  end

end
