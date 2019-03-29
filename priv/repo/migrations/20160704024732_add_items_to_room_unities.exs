defmodule ApathyDrive.Repo.Migrations.AddItemsToRoomUnities do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      remove(:items)
    end

    alter table(:room_unities) do
      add(:items, :jsonb)
    end
  end
end
