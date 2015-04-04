defmodule ApathyDrive.Repo.Migrations.AddAcToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add :ac, :integer
    end
  end

  def down do
    alter table(:monster_templates) do
      remove :ac
    end
  end
end
