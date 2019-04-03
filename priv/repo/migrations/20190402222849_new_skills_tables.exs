defmodule ApathyDrive.Repo.Migrations.NewSkillsTables do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      remove(:training_cost_multiplier)
    end

    drop(table(:rooms_skills))
  end
end
