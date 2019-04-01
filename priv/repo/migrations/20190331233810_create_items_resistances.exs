defmodule ApathyDrive.Repo.Migrations.CreateItemsResistances do
  use Ecto.Migration

  def change do
    create table(:items_resistances) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:damage_type_id, references(:damage_types, on_delete: :delete_all))
      add(:amount, :integer)
    end

    create(index(:items_resistances, :item_id))
    create(index(:items_resistances, :damage_type_id))
  end
end
