defmodule ApathyDrive.Repo.Migrations.AddLastKilledAtToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add :last_killed_at, :datetime
    end
  end
end
