defmodule ApathyDrive.Repo.Migrations.RemoveCastingSkillIdFromSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      remove(:casting_skill_id)
    end

    alter table(:trainers) do
      remove(:level)
    end
  end
end
