defmodule ApathyDrive.Repo.Migrations.CreateSkillTables do
  use Ecto.Migration

  def change do
    create table(:skills) do
      add :name, :text
      add :training_cost_multiplier, :float, default: 1.0     
    end

    create table(:rooms_skills) do
      add :room_id, references(:rooms)
      add :skill_id, references(:skills)
    end

    create table(:characters_skills) do
      add :character_id, references(:characters)
      add :skill_id, references(:skills)
      add :experience, :bigint, default: 0
      add :level, :integer, default: 1
    end

    create table(:skills_relationships) do
      add :skill_id, references(:skills)
      add :related_skill_id, references(:skills)
      add :relationship_type, :text
    end
  end
end
