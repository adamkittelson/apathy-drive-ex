defmodule ApathyDrive.Repo.Migrations.CreateRoomsPlacedItems do
  use Ecto.Migration

  def change do
    create table(:rooms_placed_items) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:next_spawn_at, :utc_datetime)
    end
  end
end
