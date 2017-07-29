defmodule ApathyDrive.Repo.Migrations.CreateRoomsItems do
  use Ecto.Migration

  def change do
    create table(:rooms_items) do
      add :room_id, references(:rooms)
      add :item_id, references(:items)
      add :level, :integer

      timestamps
    end
  end
end
