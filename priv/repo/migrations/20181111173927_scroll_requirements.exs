defmodule ApathyDrive.Repo.Migrations.ScrollRequirements do
  use Ecto.Migration

  def up do
    alter table(:classes_abilities) do
      remove(:required_strength)
      remove(:required_agility)
      remove(:required_intellect)
      remove(:required_willpower)
      remove(:required_health)
      remove(:required_charm)
    end
  end

  def down do
    alter table(:classes_abilities) do
      add(:required_strength, :integer)
      add(:required_agility, :integer)
      add(:required_intellect, :integer)
      add(:required_willpower, :integer)
      add(:required_health, :integer)
      add(:required_charm, :integer)
    end
  end
end
