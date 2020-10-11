defmodule ApathyDrive.Repo.Migrations.CreatePowerLevels do
  use Ecto.Migration

  def change do
    create table(:power_levels) do
      add(:name, :text)
      add(:hp_multiplier, :float)
      add(:damage_multiplier, :float)
    end

    create table(:monsters_power_levels) do
      add(:monster_id, references(:monsters, on_delete: :delete_all))
      add(:power_level_id, references(:power_levels, on_delete: :delete_all))
    end
  end
end
