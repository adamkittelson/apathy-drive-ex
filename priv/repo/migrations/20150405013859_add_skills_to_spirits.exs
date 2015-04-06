defmodule ApathyDrive.Repo.Migrations.AddSkillsToSpirits do
  use Ecto.Migration

  def up do
    alter table(:spirits) do
      add :skills, :jsonb
    end
  end

  def down do
    alter table(:spirits) do
      remove :skills
    end
  end
end
