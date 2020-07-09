defmodule ApathyDrive.Repo.Migrations.AddEvilPointsLastReducedAtToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:evil_points_last_reduced_at, :utc_datetime_usec)
      remove(:last_evil_action_at)
    end
  end
end
