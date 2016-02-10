defmodule ApathyDrive.Repo.Migrations.AddPermanentToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add :permanent, :boolean
    end
  end
end
