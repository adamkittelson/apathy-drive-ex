defmodule ApathyDrive.Repo.Migrations.AddMaxHpToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add(:max_hp, :integer)
    end
  end

  def down do
    alter table(:monster_templates) do
      remove(:max_hp)
    end
  end
end
