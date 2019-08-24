defmodule ApathyDrive.Repo.Migrations.CharacterAlignmentTweaks do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:alignment)
      remove(:bounty)
      add(:evil_points, :float, default: 0)
      add(:last_evil_action_at, :utc_datetime)
    end
  end
end
