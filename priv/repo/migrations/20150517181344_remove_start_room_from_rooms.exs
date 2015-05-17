defmodule ApathyDrive.Repo.Migrations.RemoveStartRoomFromRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      remove :start_room
    end
  end
end
