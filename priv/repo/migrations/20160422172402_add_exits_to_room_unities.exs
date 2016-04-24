defmodule ApathyDrive.Repo.Migrations.AddExitsToRoomUnities do
  use Ecto.Migration

  def change do
    alter table(:room_unities) do
      add :exits, :jsonb
    end
  end
end
