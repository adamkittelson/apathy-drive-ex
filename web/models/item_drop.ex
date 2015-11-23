defmodule ApathyDrive.ItemDrop do
  use ApathyDrive.Web, :model

  schema "item_drops" do
    field :monster_id, :integer
    field :item_id, :integer
    field :chance, :integer

    timestamps
  end

  @required_fields ~w(monster_id item_id chance)
  @optional_fields ~w()

  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> unique_constraint(:item_id, name: :item_drops_monster_id_item_id_index)
    |> foreign_key_constraint(:item_id)
    |> foreign_key_constraint(:monster_id)
  end

  def monster_drops(monster_template_id) do
    query = from drop in __MODULE__,
            where: drop.monster_id == ^monster_template_id,
            select: drop

    query
    |> Repo.all
  end

  def item_drops(item_id) do
    query = from drop in __MODULE__,
            where: drop.item_id == ^item_id,
            select: drop

    query
    |> Repo.all
  end

  def names(drops) when is_list(drops) do
    drops
    |> Enum.map(&Map.from_struct/1)
    |> Enum.map(&names/1)
  end

  def names(%{item_id: item_id, monster_id: monster_id} = drop) do
    drop
    |> Map.put(:item, Repo.get(ApathyDrive.Item, item_id).name)
    |> Map.put(:monster, Repo.get(MonsterTemplate, monster_id).name)
  end

end
