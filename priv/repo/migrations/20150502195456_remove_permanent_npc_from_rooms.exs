defmodule ApathyDrive.Repo.Migrations.RemovePermanentNpcFromRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      remove :permanent_npc
    end
  end
end
