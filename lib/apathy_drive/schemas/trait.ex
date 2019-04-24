defmodule ApathyDrive.Trait do
  use ApathyDriveWeb, :model

  schema "traits" do
    field(:name, :string)
    field(:description, :string)

    has_many(:monsters_traits, ApathyDrive.MonsterTrait)
    has_many(:monsters, through: [:monsters_traits, :monster])

    has_many(:races_traits, ApathyDrive.MonsterTrait)
    has_many(:races, through: [:races_traits, :race])

    timestamps()
  end

  def names do
    __MODULE__
    |> Ecto.Query.select([:name])
    |> Repo.all()
    |> Enum.map(& &1.name)
    |> Enum.sort()
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :name])
    |> Enum.sort_by(& &1.name)
    |> Enum.map(&{&1.name, &1.id})
  end

  def merge_traits(traits1, traits2) do
    Enum.reduce(traits2, traits1, fn {trait, value}, traits ->
      if trait in Map.keys(traits) do
        Map.put(traits, trait, [value | List.wrap(traits[trait])])
      else
        Map.put(traits, trait, value)
      end
    end)
  end

  def value(_trait, list) do
    list = List.flatten(list)

    Enum.sum(list)
  end
end
