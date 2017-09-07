defmodule ApathyDrive.MonsterTrait do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Monster, Trait}

  schema "monsters_traits" do
    field :value, ApathyDrive.JSONB

    belongs_to :monster, Monster
    belongs_to :trait, Trait
  end

  def load_traits(monster_id) do
    __MODULE__
    |> where([mt], mt.monster_id == ^monster_id)
    |> preload([:trait])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
         Map.put(abilities, trait.name, value)
       end)
  end

end
