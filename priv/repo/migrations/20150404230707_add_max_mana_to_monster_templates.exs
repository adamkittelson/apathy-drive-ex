defmodule ApathyDrive.Repo.Migrations.AddMaxManaToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add :max_mana, :integer
      add :hp_regen, :integer
      add :mana_regen, :integer
    end
  end

  def down do
    alter table(:monster_templates) do
      remove :max_mana
      remove :hp_regen
      remove :mana_regen
    end
  end
end
