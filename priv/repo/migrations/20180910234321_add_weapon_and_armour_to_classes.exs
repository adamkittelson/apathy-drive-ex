defmodule ApathyDrive.Repo.Migrations.AddWeaponAndArmourToClasses do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      add(:weapon, :text)
      add(:armour, :text)
    end
  end
end
