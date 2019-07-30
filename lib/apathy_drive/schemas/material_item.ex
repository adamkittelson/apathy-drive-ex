defmodule ApathyDrive.MaterialItem do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Item, ItemInstance, Material, Room, Skill}

  schema "materials_items" do
    field(:level, :integer)
    field(:amount, :integer)

    belongs_to(:material, Material)
    belongs_to(:item, Item)
    belongs_to(:skill, Skill)
  end

  def load_items(%Room{id: id}) do
    __MODULE__
    |> where([ri], ri.room_id == ^id)
    |> preload(:item)
    |> Repo.all()
    |> Enum.map(&Item.from_assoc/1)
  end

  def load_items(%Character{id: id}) do
    __MODULE__
    |> where([ri], ri.character_id == ^id)
    |> preload(:item)
    |> Repo.all()
    |> Enum.map(&Item.from_assoc/1)
  end

  def drop_loot_for_character(%Room{} = room, %Character{level: level} = character) do
    count =
      __MODULE__
      |> where([mi], mi.level == ^level)
      |> select([mi], count(mi.id))
      |> Repo.one()

    mi =
      __MODULE__
      |> where([mi], mi.level == ^level)
      |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
      |> select([mi], mi)
      |> Repo.one()

    %ItemInstance{
      item_id: mi.item_id,
      room_id: room.id,
      level: mi.level,
      character_id: nil,
      dropped_for_character_id: character.id,
      equipped: false,
      hidden: false,
      delete_at: Timex.shift(DateTime.utc_now(), hours: 1)
    }
    |> Repo.insert!()

    room
  end
end
