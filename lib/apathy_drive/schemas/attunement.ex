defmodule ApathyDrive.Attunement do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, ItemInstance}

  schema "characters_attunements" do
    belongs_to(:character, Character)
    belongs_to(:obelisk, ItemInstance)
  end

  def load(character) do
    attunements =
      __MODULE__
      |> Ecto.Query.where(character_id: ^character.id)
      |> Ecto.Query.preload(obelisk: [room: :area])
      |> Repo.all()
      |> Enum.map(fn attunement ->
        %{
          room_id: attunement.obelisk.room_id,
          coordinates: attunement.obelisk.room.coordinates,
          name: attunement.obelisk.room.area.name
        }
      end)
      |> IO.inspect()

    put_in(character.attunements, attunements)
  end
end
