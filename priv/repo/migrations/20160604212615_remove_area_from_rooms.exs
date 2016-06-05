defmodule ApathyDrive.Repo.Migrations.RemoveAreaFromRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      remove :area
    end
  end
end
