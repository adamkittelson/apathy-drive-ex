defmodule ApathyDrive.Repo.Migrations.AddBeaconRoomIdToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:beacon_room_id, references(:rooms, on_delete: :delete_all))
    end

    create(index(:items_instances, :beacon_room_id))
  end
end
