defmodule ApathyDrive.Repo.Migrations.CreateRoomUnities do
  use Ecto.Migration

  def change do
    create table(:room_unities) do
      add(:unity, :text)
      add(:essences, :jsonb)
      add(:room_id, :integer)
      add(:expires_at, :utc_datetime)

      timestamps
    end

    create(index(:room_unities, [:room_id]))

    alter table(:rooms) do
      remove(:lair_faction)
    end
  end
end
