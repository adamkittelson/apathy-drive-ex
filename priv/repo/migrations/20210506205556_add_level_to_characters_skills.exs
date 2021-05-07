defmodule ApathyDrive.Repo.Migrations.AddLevelToCharactersSkills do
  use Ecto.Migration

  def change do
    alter table(:characters_skills) do
      add(:level, :integer)
    end
  end
end
