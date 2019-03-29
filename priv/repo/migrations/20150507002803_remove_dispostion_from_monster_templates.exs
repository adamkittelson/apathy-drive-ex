defmodule ApathyDrive.Repo.Migrations.RemoveDispostionFromMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      remove(:disposition)
    end
  end
end
