defmodule ApathyDrive.Repo.Migrations.AbilityTimings do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:cast_time, :integer)
    end

    rename(table(:abilities), :duration_in_ms, to: :duration)
    rename(table(:abilities), :cooldown_in_ms, to: :cooldown)
  end
end
