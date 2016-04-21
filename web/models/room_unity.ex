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

end
