defmodule ApathyDrive.Repo.Migrations.AddTypeIdToItems do
  use Ecto.Migration

  def change do
    drop(table(:items_item_types))

    alter table(:items) do
      add(:type_id, references(:item_types, on_delete: :nilify_all))
    end
  end
end
