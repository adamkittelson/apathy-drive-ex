defmodule ApathyDrive.EntityAbility do
  use ApathyDrive.Web, :model

  schema "entities_abilities" do
    field :assoc_table, :string
    field :assoc_id, :integer
    field :ability, :string
    field :value, ApathyDrive.JSONB

    timestamps()
  end

  def load_abilities(table, id) do
    __MODULE__
    |> where([ea], ea.assoc_table == ^table and ea.assoc_id == ^id)
    |> Repo.all
    |> Enum.reduce(%{}, fn %{ability: ability, value: value}, abilities ->
         Map.put(abilities, ability, value)
       end)
  end

end
