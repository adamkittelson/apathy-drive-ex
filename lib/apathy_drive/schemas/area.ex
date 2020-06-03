defmodule ApathyDrive.Area do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Area, Match, Room}

  schema "areas" do
    field(:name, :string)
    field(:level, :integer, default: 1)
    field(:map, ApathyDrive.JSONB, default: %{})

    has_many(:rooms, ApathyDrive.Room)
    has_many(:lair_monsters, through: [:rooms, :lairs, :monster_template])

    timestamps()
  end

  def without_map() do
    from(Area, select: [:id, :name, :level])
  end

  def update_map!() do
    Room.world_map()
    |> Repo.all()
    |> Enum.each(fn %Area{} = area ->
      update_area_map!(area)
    end)

    send(ApathyDrive.WorldMap, :load_world_map)
  end

  def update_area_map!(%Area{id: area_id} = area) do
    map =
      from(
        room in Room,
        where: room.area_id == ^area_id and not is_nil(room.coordinates),
        select: %{
          id: room.id,
          name: room.name,
          coords: room.coordinates
        }
      )
      |> Repo.all()
      |> Enum.reduce(%{}, fn %{id: id} = room, map ->
        directions =
          ApathyDrive.RoomExit.load_exits(id)
          |> Enum.filter(
            &(&1["kind"] in [
                "Block Guard",
                "Normal",
                "Action",
                "Class",
                "Door",
                "Gate",
                "Trap",
                "Cast",
                "Level",
                "Toll",
                "Alignment",
                "Key"
              ])
          )
          |> Enum.map(& &1["direction"])

        room =
          room
          |> Map.put(:directions, directions)
          |> Map.delete(:exits)

        Map.put(map, to_string(id), room)
      end)

    area
    |> Ecto.Changeset.change(%{
      map: map
    })
    |> Repo.update!()
  end

  def find_by_name(name) do
    __MODULE__
    |> where(name: ^name)
  end

  def match_by_name(name) do
    __MODULE__
    |> where([area], not is_nil(area.name) and area.name != "")
    |> distinct(true)
    |> select([area], [:id, :name])
    |> ApathyDrive.Repo.all()
    |> Match.one(:keyword_starts_with, name)
  end

  def list_with_room_counts do
    from(
      room in Room,
      where: not is_nil(room.coordinates),
      join: area in assoc(room, :area),
      group_by: area.id,
      order_by: [desc: area.level, desc: count(area.id), asc: area.name],
      select: [area, count(area.id)]
    )
  end

  def set_levels! do
    list_with_room_counts()
    |> ApathyDrive.Repo.all()
    |> Enum.each(fn [area, _count] ->
      max =
        area
        |> Ecto.assoc(:lair_monsters)
        |> ApathyDrive.Repo.all()
        |> Enum.map(& &1.level)
        |> List.insert_at(0, 1)
        |> Enum.max()

      area
      |> Map.put(:level, max)
      |> ApathyDrive.Repo.save!()
    end)
  end

  def changeset(%Area{} = area, params \\ %{}) do
    area
    |> cast(params, ~w(name level hostile), ~w())
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
  end

  def new_area_changeset(name) do
    %__MODULE__{}
    |> cast(%{name: name}, [:name])
    |> validate_required(:name)
    |> validate_format(:name, ~r/^[a-zA-Z\d ,\-']+$/)
    |> validate_length(:name, min: 1, max: 20)
    |> unique_constraint(:name)
  end

  def update_level(%Area{} = area, level) when is_integer(level) do
    area
    |> Map.put(:level, level)
    |> Repo.save!()
  end

  def update_level(area, level), do: update_level(area, String.to_integer(level))
end
