defmodule ApathyDrive.Repo.Migrations.RemoveLastKilledAtAndRegenTimeFromMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      remove(:last_killed_at)
      remove(:regen_time_in_minutes)
    end
  end
end
