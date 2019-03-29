defmodule ApathyDrive.Repo.Migrations.RemoveExitsFromRoomUnities do
  use Ecto.Migration

  def change do
    alter table(:room_unities) do
      remove(:exits)
    end
  end
end
