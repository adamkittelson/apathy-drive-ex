defmodule ApathyDrive.Repo.Migrations.AddSpawnedAtToMobiles do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      add :spawned_at, :integer
    end
  end
end
