defmodule ApathyDrive.SpiritItemRecipe do
  use ApathyDrive.Web, :model
  alias ApathyDrive.MonsterTemplate

  schema "spirit_item_recipes" do
    belongs_to :spirit, Spirit
    belongs_to :item, ApathyDrive.Item

    timestamps()
  end

  @required_fields ~w(spirit_id item_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> unique_constraint(:item_id, name: :spirit_item_recipes_spirit_id_item_id_index)
    |> foreign_key_constraint(:item_id)
    |> foreign_key_constraint(:spirit_id)
  end

  def known_recipes(spirit_id) do
    query = from recipe in __MODULE__,
            where: recipe.spirit_id == ^spirit_id,
            select: recipe

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
