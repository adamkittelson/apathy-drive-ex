defmodule ApathyDrive.Repo.Migrations.AddItemsTraits do
  use Ecto.Migration

  def change do
    create table(:items_traits) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:trait_id, references(:traits, on_delete: :delete_all))
      add(:value, :jsonb)
    end

    create(index(:items_traits, [:item_id, :trait_id], unique: true))
  end
end
