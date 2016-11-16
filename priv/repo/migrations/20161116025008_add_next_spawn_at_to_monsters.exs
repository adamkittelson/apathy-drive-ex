defmodule ApathyDrive.Repo.Migrations.AddNextSpawnAtToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add :next_spawn_at, :bigint
    end
  end
end
