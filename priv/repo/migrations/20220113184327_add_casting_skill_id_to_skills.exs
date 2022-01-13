defmodule ApathyDrive.Repo.Migrations.AddCastingSkillIdToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:casting_skill_id, references(:skills, on_delete: :nilify_all))
    end
  end
end
