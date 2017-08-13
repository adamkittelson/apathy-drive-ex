defmodule ApathyDrive.Repo.Migrations.CreateSkillTables do
  use Ecto.Migration

  def change do
    create table(:skills) do
      add :name, :text
      add :training_cost_multiplier, :float, default: 1.0
      add :help, :text
    end

    create table(:rooms_skills) do
      add :room_id, references(:rooms)
      add :skill_id, references(:skills)
    end

    create table(:characters_skills) do
      add :character_id, references(:characters)
      add :skill_id, references(:skills)
      add :experience, :bigint, default: 0
    end

    create table(:skills_incompatibilities) do
      add :skill_id, references(:skills)
      add :incompatible_skill_id, references(:skills)
    end
  end
end
