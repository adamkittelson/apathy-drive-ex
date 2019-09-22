defmodule ApathyDrive.RoomMonster do
  use ApathyDriveWeb, :model

  @primary_key {:id, :id, autogenerate: true}
  schema "rooms_monsters" do
    belongs_to(:room, ApathyDrive.Room)
    belongs_to(:monster, ApathyDrive.Monster)
    belongs_to(:character, ApathyDrive.Character)
    field(:strength, :integer)
    field(:agility, :integer)
    field(:intellect, :integer)
    field(:willpower, :integer)
    field(:health, :integer)
    field(:charm, :integer)
    field(:level, :integer)
    field(:spawned_at, :integer)
    field(:zone_spawned_at, :integer)
    field(:name, :string)
    field(:decay, :boolean)
    field(:missing_limbs, {:array, :string}, default: [])

    timestamps()
  end
end
