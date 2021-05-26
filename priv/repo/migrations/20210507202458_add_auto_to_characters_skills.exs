defmodule ApathyDrive.Repo.Migrations.AddAutoToCharactersSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:command, :text)
    end

    alter table(:characters_skills) do
      add(:auto, :boolean, default: false)
    end
  end
end
