defmodule ApathyDrive.Repo.Migrations.AddCommandsToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add(:commands, :jsonb)
    end
  end
end
