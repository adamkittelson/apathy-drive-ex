defmodule ApathyDrive.RoomUnity do
  use ApathyDrive.Web, :model
  use Timex

  schema "room_unities" do
    field :controlled_by, :string
    field :essences,   ApathyDrive.JSONB, default: %{}
    field :expires_at, Timex.Ecto.DateTime

    timestamps

    belongs_to :room, Room
  end

  def room_ids do
    __MODULE__
    |> distinct(true)
    |> select([ru], ru.room_id)
  end

  def controlled_by_counts do
    __MODULE__
    |> group_by([ru], ru.controlled_by)
    |> select([ru], {ru.controlled_by, count(ru.controlled_by)})
    |> Repo.all
  end

end
