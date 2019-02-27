defmodule ApathyDrive.Repo.Migrations.FixChannelHistoryConstraint do
  use Ecto.Migration

  def change do
    execute("ALTER TABLE channel_history DROP CONSTRAINT channel_history_character_id_fkey")
  end
end
