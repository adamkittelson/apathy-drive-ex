defmodule ApathyDrive.Repo.Migrations.AddWeaponFieldsToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:kind, :string)
      add(:speed, :integer)
      add(:weight, :integer)
      add(:min_damage, :integer)
      add(:max_damage, :integer)
      add(:required_strength, :integer, default: 0)
      add(:required_agility, :integer, default: 0)
      add(:required_intellect, :integer, default: 0)
      add(:required_willpower, :integer, default: 0)
      add(:required_health, :integer, default: 0)
      add(:required_charm, :integer, default: 0)
    end
  end
end
