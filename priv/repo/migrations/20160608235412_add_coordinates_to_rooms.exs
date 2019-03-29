defmodule ApathyDrive.Repo.Migrations.AddCoordinatesToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add(:coordinates, :jsonb)
    end
  end
end
