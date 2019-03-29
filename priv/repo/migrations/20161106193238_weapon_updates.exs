defmodule ApathyDrive.Repo.Migrations.WeaponUpdates do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:hit_verbs, :jsonb)
      add(:miss_verbs, :jsonb)
      remove(:strength)
      remove(:intellect)
      remove(:willpower)
      remove(:agility)
      remove(:health)
      remove(:charm)
    end

    alter table(:characters_items) do
      add(:strength, :integer)
      add(:intellect, :integer)
      add(:willpower, :integer)
      add(:agility, :integer)
      add(:health, :integer)
      add(:charm, :integer)
    end

    alter table(:rooms_items) do
      add(:strength, :integer)
      add(:intellect, :integer)
      add(:willpower, :integer)
      add(:agility, :integer)
      add(:health, :integer)
      add(:charm, :integer)
    end
  end
end
