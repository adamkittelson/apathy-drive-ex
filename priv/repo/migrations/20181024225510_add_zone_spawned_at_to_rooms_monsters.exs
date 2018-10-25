defmodule ApathyDrive.Repo.Migrations.AddZoneSpawnedAtToRoomsMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add(:zone_spawned_at, references(:rooms, on_delete: :delete_all))
    end
  end
end
