defmodule ApathyDrive.Repo.Migrations.AddItemsToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add :items, :jsonb
    end
  end
end
