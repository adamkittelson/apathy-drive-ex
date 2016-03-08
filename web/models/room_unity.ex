defmodule ApathyDrive.RoomUnity do
  use ApathyDrive.Web, :model
  use Timex

  schema "room_unities" do
    field :unity,      :string
    field :essences,   ApathyDrive.JSONB, default: %{}
    field :expires_at, Timex.Ecto.DateTime

    timestamps

    belongs_to :room, Room
  end
end