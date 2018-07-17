defmodule ApathyDrive.Repo.Migrations.AddAttributeExperienceToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:experience)
      remove(:level)
      add(:strength_experience, :bigint, default: 0)
      add(:agility_experience, :bigint, default: 0)
      add(:intellect_experience, :bigint, default: 0)
      add(:willpower_experience, :bigint, default: 0)
      add(:health_experience, :bigint, default: 0)
      add(:charm_experience, :bigint, default: 0)
    end
  end
end
