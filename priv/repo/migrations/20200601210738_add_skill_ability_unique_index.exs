defmodule ApathyDrive.Repo.Migrations.AddSkillAbilityUniqueIndex do
  use Ecto.Migration

  def change do
    create(unique_index(:skills_abilities, [:skill_id, :ability_id]))
  end
end
