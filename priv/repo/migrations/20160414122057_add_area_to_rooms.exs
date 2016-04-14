defmodule ApathyDrive.Repo.Migrations.AddAreaToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add :area, :text
    end
  end
end
