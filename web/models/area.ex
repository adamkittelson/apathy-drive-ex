defmodule ApathyDrive.Area do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Area, Match, Room}

  schema "areas" do
    field :name, :string
    field :level, :integer, default: 1

    has_many :rooms, ApathyDrive.Room
    has_many :lair_monsters, through: [:rooms, :lairs, :monster_template]

    timestamps
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
    |> ApathyDrive.Repo.all
    |> Match.one(:keyword_starts_with, name)
  end

  def list_with_room_counts do
    from room in Room,
    where: not is_nil(room.coordinates),
    join: area in assoc(room, :area),
    group_by: area.id,
    order_by: [desc: area.level, desc: count(area.id), asc: area.name],
    select: [area, count(area.id)]
  end

  def set_levels! do
    list_with_room_counts
    |> ApathyDrive.Repo.all
    |> Enum.each(fn [area, _count] ->
         max =
           area
           |> Ecto.assoc(:lair_monsters)
           |> ApathyDrive.Repo.all
           |> Enum.map(&(&1.level))
           |> List.insert_at(0, 1)
           |> Enum.max

         area
         |> Map.put(:level, max)
         |> ApathyDrive.Repo.save!
       end)
  end

  def changeset(name) do
    %__MODULE__{}
    |> cast(%{name: name}, ~w(name))
    |> validate_required(:name)
    |> validate_format(:name, ~r/^[a-zA-Z\d ,\-']+$/)
    |> validate_length(:name, min: 1, max: 20)
    |> unique_constraint(:name)
  end

  def update_level(%Area{} = area, level) when is_integer(level) do
    area
    |> Map.put(:level, level)
    |> Repo.save!
  end
  def update_level(area, level), do: update_level(area, String.to_integer(level))

end
