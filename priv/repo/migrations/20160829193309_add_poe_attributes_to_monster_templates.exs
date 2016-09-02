defmodule ApathyDrive.Repo.Migrations.AddPoeAttributesToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add :abilities, :jsonb

      remove :permanent
    end
  end

  def down do
    alter table(:monster_templates) do
      remove :abilities

      add :permanent, :boolean
    end
  end
end
