defmodule ApathyDrive.Repo.Migrations.AddOwnerIdToItemsInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:owner_id, references(:characters, on_delete: :nilify_all))
    end
  end
end
