defmodule ApathyDrive.Repo.Migrations.AddEvilPointsToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:evil_points, :float, default: 0.0)
    end
  end
end
