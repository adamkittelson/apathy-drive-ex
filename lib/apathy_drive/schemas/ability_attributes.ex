defmodule ApathyDrive.AbilityAttribute do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, Attribute}

  schema "abilities_attributes" do
    belongs_to(:ability, Ability)
    belongs_to(:attribute, Attribute)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(attribute_id ability_id)a)
  end

  def load_attributes(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload(:attribute)
    |> Repo.all()
    |> Enum.reduce([], fn %{attribute: %{name: attribute}}, attributes ->
      [attribute | attributes]
    end)
  end
end
