defmodule ApathyDrive.Repo.Migrations.ChangeAbilityRequirements do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:level, :integer)
      add(:school, :text)
      remove(:required_skills)
    end
  end
end
