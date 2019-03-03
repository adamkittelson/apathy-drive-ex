defmodule ApathyDrive.Repo.Migrations.RemoveItemDescriptionsFromRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      remove(:item_descriptions)
    end
  end
end
