defmodule ApathyDrive.Repo.Migrations.AddRegenTimeToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add(:regen_time_in_minutes, :integer)
    end
  end
end
