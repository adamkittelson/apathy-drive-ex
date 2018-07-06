defmodule ApathyDrive.Repo.Migrations.AddClassesAbilities do
  use Ecto.Migration

  def change do
    create table(:classes_abilities) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:ability_id, references(:abilities, on_delete: :delete_all))
      add(:level, :integer)
      add(:required_strength, :integer)
      add(:required_agility, :integer)
      add(:required_intellect, :integer)
      add(:required_willpower, :integer)
      add(:required_health, :integer)
      add(:required_charm, :integer)
    end
  end
end
