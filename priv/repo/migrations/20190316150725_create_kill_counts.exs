defmodule ApathyDrive.Repo.Migrations.CreateKillCounts do
  use Ecto.Migration

  def change do
    create table(:kill_counts) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:monster_id, references(:monsters, on_delete: :delete_all))
      add(:daily_count, :bigint)
      add(:daily_reset_at, :utc_datetime)
      add(:weekly_count, :bigint)
      add(:weekly_reset_at, :utc_datetime)
      add(:lifetime_count, :bigint)
    end

    create(index(:kill_counts, [:character_id]))
    create(index(:kill_counts, [:monster_id]))
    create(index(:kill_counts, [:character_id, :monster_id]))
  end
end
