defmodule ApathyDrive.Repo.Migrations.AddClassIdToItemsInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:class_id, references(:classes, on_delete: :delete_all))
    end
  end
end
