defmodule ApathyDrive.LairMonster do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Room

  schema "lair_monsters" do
    belongs_to :room, Room
    belongs_to :monster_template, MonsterTemplate

    timestamps
  end

  @required_fields ~w(room_id monster_template_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    updated_params = update_params(params)

    model
    |> cast(updated_params, @required_fields, @optional_fields)
    |> unique_constraint(:monster_id, name: :lair_monsters_room_id_monster_template_id_index)
    |> foreign_key_constraint(:room_id)
    |> foreign_key_constraint(:monster_template_id)
  end

  def update_params(:empty), do: :empty
  def update_params(params) do
    params
    |> Map.put("monster_template_id", get_number(Map.get(params, "monster_template_id")))
    |> Map.put("room_id", get_number(Map.get(params, "room_id")))
  end

  def get_number(nil), do: nil
  def get_number(""),  do: nil
  def get_number(string) do
    case Regex.run(~r/\d+$/, string) do
      nil ->
        nil
      [number] ->
        number
    end
  end

  def monsters_template_ids(room_id) do
    query = from lair in __MODULE__,
            where: lair.room_id == ^room_id,
            select: lair.monster_template_id

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
