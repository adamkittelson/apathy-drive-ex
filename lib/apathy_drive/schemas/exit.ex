defmodule ApathyDrive.Exit do
  use ApathyDriveWeb, :model

  schema "exits" do
    field(:kind, :string)

    has_many(:rooms_exits, ApathyDrive.RoomExit)
    has_many(:rooms, through: [:rooms_exits, :room])
  end

  def kinds do
    __MODULE__
    |> Ecto.Query.select([:kind])
    |> Repo.all()
    |> Enum.map(& &1.kind)
    |> Enum.sort()
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :kind])
    |> Enum.sort_by(& &1.kind)
    |> Enum.map(&{&1.kind, &1.id})
  end

  def direction_description(direction) do
    case direction do
      "up" ->
        "above you"

      "down" ->
        "below you"

      direction ->
        "to the #{direction}"
    end
  end

  def reverse_direction("north"), do: "south"
  def reverse_direction("northeast"), do: "southwest"
  def reverse_direction("east"), do: "west"
  def reverse_direction("southeast"), do: "northwest"
  def reverse_direction("south"), do: "north"
  def reverse_direction("southwest"), do: "northeast"
  def reverse_direction("west"), do: "east"
  def reverse_direction("northwest"), do: "southeast"
  def reverse_direction("up"), do: "down"
  def reverse_direction("down"), do: "up"
end
