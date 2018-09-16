defmodule ApathyDrive.Repo.Migrations.ShopChanges do
  use Ecto.Migration

  def change do
    create table(:shops) do
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:cost_multiplier, :float)
    end

    alter table(:items_instances) do
      add(:shop_id, references(:shops, on_delete: :delete_all))
    end

    alter table(:shop_items) do
      remove(:room_id)
      add(:shop_id, references(:shops, on_delete: :delete_all))
      remove(:max_stock)
      remove(:stock)
      remove(:regen_frequency_in_minutes)
      remove(:regen_amount)
      remove(:next_regen_at)

      add(:stock, :integer)
      add(:restock_frequency_in_minutes, :integer)
      add(:restock_chance, :integer)
      add(:restock_amount, :integer)
      add(:next_restock_at, :utc_datetime)
    end

    create(unique_index(:shop_items, [:shop_id, :item_id]))
  end
end
