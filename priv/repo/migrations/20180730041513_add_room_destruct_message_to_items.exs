defmodule ApathyDrive.Repo.Migrations.AddRoomDestructMessageToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:room_destruct_message, :text)
    end
  end
end
