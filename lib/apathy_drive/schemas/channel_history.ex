defmodule ApathyDrive.ChannelHistory do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Repo}

  schema "channel_history" do
    belongs_to(:character, Character)
    field(:character_name, :string)
    field(:channel_name, :string)
    field(:game_name, :string)
    field(:message, :string)

    timestamps()
  end

  def fetch(rows \\ 100) do
    __MODULE__
    |> Ecto.Query.order_by(asc: :id)
    |> Ecto.Query.limit(^rows)
    |> Ecto.Query.select([ch], ch.message)
    |> Repo.all()
  end
end
