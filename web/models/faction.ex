defmodule ApathyDrive.Faction do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Faction, Match}

  schema "factions" do
    field :name, :string
    field :relationships, :map, default: %{"allies" => [], "enemies" => []}

    timestamps
  end

  def create_relationship(faction, other_faction, "Ally") do
    faction = find_by_name(faction)
    other_faction = find_by_name(other_faction)

    if faction && other_faction do
      update_in(faction.relationships["allies"], &(Enum.uniq([other_faction.name | &1])))
      |> Repo.save!

      update_in(other_faction.relationships["allies"], &(Enum.uniq([faction.name | &1])))
      |> Repo.save!

      {:ok, faction.name, other_faction.name}
    else
      :error
    end
  end

  def create_relationship(faction, other_faction, "Enemy") do
    faction = find_by_name(faction)
    other_faction = find_by_name(other_faction)

    if faction && other_faction do
      update_in(faction.relationships["enemies"], &(Enum.uniq([other_faction.name | &1])))
      |> Repo.save!

      update_in(other_faction.relationships["enemies"], &(Enum.uniq([faction.name | &1])))
      |> Repo.save!

      {:ok, faction.name, other_faction.name}
    else
      :error
    end
  end

  def new_faction_changeset(name) do
    %__MODULE__{}
    |> cast(%{name: name}, ~w(name))
    |> validate_required(:name)
    |> validate_format(:name, ~r/^[a-zA-Z\d ,\-']+$/)
    |> validate_length(:name, min: 1, max: 20)
    |> unique_constraint(:name)
  end

  def find_by_name(name) do
    Faction
    |> where([faction], not is_nil(faction.name) and faction.name != "")
    |> distinct(true)
    |> ApathyDrive.Repo.all
    |> Match.one(:keyword_starts_with, name)
  end


end
