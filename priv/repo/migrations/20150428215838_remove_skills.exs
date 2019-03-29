defmodule ApathyDrive.Repo.Migrations.RemoveSkills do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      remove(:trainable_skills)
    end

    drop(table(:skills))
  end
end
