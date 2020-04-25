defmodule ApathyDrive.Repo.Migrations.CreateClassesSkills do
  use Ecto.Migration

  def change do
    create table(:classes_skills) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
    end

    create(index(:classes_skills, :class_id))
    create(index(:classes_skills, :skill_id))
    create(index(:classes_skills, [:class_id, :skill_id], unique: true))
  end
end
