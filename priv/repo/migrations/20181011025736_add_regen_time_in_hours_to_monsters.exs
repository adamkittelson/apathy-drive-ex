defmodule ApathyDrive.Repo.Migrations.AddRegenTimeInHoursToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:regen_time_in_hours, :integer)
    end
  end
end
