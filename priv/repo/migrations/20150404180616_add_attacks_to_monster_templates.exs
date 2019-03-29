defmodule ApathyDrive.Repo.Migrations.AddAttacksToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add(:attacks, :jsonb)
    end
  end

  def down do
    alter table(:monster_templates) do
      remove(:attacks)
    end
  end
end
