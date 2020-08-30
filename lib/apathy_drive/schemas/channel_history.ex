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

  def fetch(character_id, rows \\ 100) do
    announce =
      __MODULE__
      |> Ecto.Query.where(
        [row],
        row.channel_name == "announce" and
          (row.character_id == ^character_id or is_nil(row.character_id))
      )
      |> Ecto.Query.order_by(desc: :id)
      |> Ecto.Query.limit(^rows)
      |> Ecto.Query.select([ch], %{
        id: ch.id,
        message: ch.message,
        time: ch.inserted_at,
        channel: ch.channel_name
      })
      |> Repo.all()

    non_announce =
      __MODULE__
      |> Ecto.Query.where(
        [row],
        row.channel_name != "announce" and
          (row.character_id == ^character_id or is_nil(row.character_id))
      )
      |> Ecto.Query.order_by(desc: :id)
      |> Ecto.Query.limit(^rows)
      |> Ecto.Query.select([ch], %{
        id: ch.id,
        message: ch.message,
        time: ch.inserted_at,
        channel: ch.channel_name
      })
      |> Repo.all()

    (announce ++ non_announce)
    |> Enum.sort_by(& &1.id, &>=/2)
  end
end
