defmodule ApathyDrive.Repo.Migrations.AddSpawnedAtToRoomsMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add :spawned_at, :integer
    end
  end
end
