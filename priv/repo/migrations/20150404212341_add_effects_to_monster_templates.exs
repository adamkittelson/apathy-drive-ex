defmodule ApathyDrive.Repo.Migrations.AddEffectsToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      remove :ac
      add :effects, :jsonb
    end
  end

  def down do
    alter table(:monster_templates) do
      add :ac, :integer
      remove :effects
    end
  end
end
