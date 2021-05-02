defmodule ApathyDrive.Repo.Migrations.CreateItemTypeParents do
  use Ecto.Migration

  def change do
    create table(:item_type_parents) do
      add(:item_type_id, references(:item_types, on_delete: :delete_all))
      add(:parent_id, references(:item_types, on_delete: :delete_all))
    end
  end
end
