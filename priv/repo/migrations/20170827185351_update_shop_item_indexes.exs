defmodule ApathyDrive.Repo.Migrations.UpdateShopItemIndexes do
  use Ecto.Migration

  def change do
    execute "ALTER TABLE shop_items DROP CONSTRAINT shop_items_room_id_fkey"
    execute "ALTER TABLE shop_items DROP CONSTRAINT shop_items_item_id_fkey"

    alter table(:shop_items) do
      modify :room_id, references(:rooms, on_delete: :delete_all)
      modify :item_id, references(:items, on_delete: :delete_all)
    end
  end
end
