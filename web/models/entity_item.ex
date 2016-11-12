defmodule ApathyDrive.EntityItem do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{EntityItem, Item}

  schema "entities_items" do
    field :assoc_table, :string
    field :assoc_id, :integer
    field :level, :integer
    field :equipped, :boolean
    field :strength, :integer
    field :agility, :integer
    field :intellect, :integer
    field :willpower, :integer
    field :health, :integer
    field :charm, :integer

    belongs_to :item, ApathyDrive.Item

    timestamps
  end

  def load_items(table, id) do
    __MODULE__
    |> where([ei], ei.assoc_table == ^table and ei.assoc_id == ^id)
    |> preload(:item)
    |> Repo.all
    |> Enum.map(&Item.from_assoc/1)
  end

end
