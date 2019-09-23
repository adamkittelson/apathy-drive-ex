defmodule ApathyDrive.Repo.Migrations.AddRoomDestructMessageToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:room_destruct_message, :text)
    end
  end
end
