defmodule ApathyDrive.Repo.Migrations.AddSchoolAndRemoveSkillsFromSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add(:school, :text)
      remove(:skills)
    end
  end
end
