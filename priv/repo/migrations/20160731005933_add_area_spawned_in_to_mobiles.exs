defmodule ApathyDrive.Repo.Migrations.AddAreaSpawnedInToMonsters do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      add(:area_spawned_in, :integer)
    end
  end
end
