defmodule ApathyDrive.Repo.Migrations.AddEnergyToAbilities do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:energy, :integer, default: 1000)
    end

    alter table(:abilities_damage_types) do
      add(:min, :integer)
      add(:max, :integer)
    end
  end
end
