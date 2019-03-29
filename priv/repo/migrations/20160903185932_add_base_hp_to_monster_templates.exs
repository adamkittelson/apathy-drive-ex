defmodule ApathyDrive.Repo.Migrations.AddBaseHpToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add(:base_hp, :integer, default: 0)
    end
  end
end
