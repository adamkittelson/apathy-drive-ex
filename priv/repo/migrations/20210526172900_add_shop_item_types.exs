defmodule ApathyDrive.Repo.Migrations.AddShopItemTypes do
  use Ecto.Migration

  def change do
    create table(:shop_item_types) do
      add(:shop_id, references(:shops, on_delete: :delete_all))
      add(:item_type_id, references(:item_types, on_delete: :delete_all))
      add(:amount, :integer)
      add(:quality, :text)
    end

    create table(:character_shops) do
      add(:shop_id, references(:shops, on_delete: :delete_all))
      add(:character_id, references(:characters, on_delete: :delete_all))

      add(:last_restocked_at, :utc_datetime)
    end

    alter table(:items_instances) do
      add(:character_shop_id, references(:character_shops, on_delete: :delete_all))
    end

    create(index(:shop_item_types, :shop_id))
    create(index(:shop_item_types, :item_type_id))

    create(index(:character_shops, :shop_id))
    create(index(:character_shops, :character_id))

    create(index(:items_instances, :character_shop_id))
  end
end
