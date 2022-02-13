defmodule ApathyDrive.Repo.Migrations.RemoveMaxLevelFromSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      remove(:max_level)
    end
  end
end
