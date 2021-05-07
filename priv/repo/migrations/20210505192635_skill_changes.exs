defmodule ApathyDrive.Repo.Migrations.SkillChanges do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      remove(:description)
      remove(:exp_multiplier)
      remove(:universal)
    end

    alter table(:characters_skills) do
      remove(:experience)
    end

    alter table(:rooms) do
      remove(:trainer_id)
    end

    drop(table(:trainers))

    create table(:trainers) do
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
    end

    drop(table(:skills_abilities))
    drop(table(:skills_attributes))
    drop(table(:skills_incompatibilities))
  end
end
