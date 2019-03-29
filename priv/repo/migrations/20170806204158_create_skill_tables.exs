defmodule ApathyDrive.Repo.Migrations.CreateSkillTables do
  use Ecto.Migration

  def change do
    create table(:skills) do
      add(:name, :text)
      add(:training_cost_multiplier, :float, default: 1.0)
      add(:description, :text)
    end

    create table(:rooms_skills) do
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
    end

    create table(:characters_skills) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:experience, :bigint, default: 0)
    end

    create table(:skills_incompatibilities) do
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:incompatible_skill_id, references(:skills, on_delete: :delete_all))
    end

    create(index(:skills, [:name], unique: true))
    create(index(:rooms_skills, [:room_id, :skill_id], unique: true))
    create(index(:characters_skills, [:character_id, :skill_id], unique: true))
    create(index(:skills_incompatibilities, [:skill_id, :incompatible_skill_id], unique: true))
  end
end
