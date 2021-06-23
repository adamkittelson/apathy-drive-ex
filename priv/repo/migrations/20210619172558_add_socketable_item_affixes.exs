defmodule ApathyDrive.Repo.Migrations.AddSocketableItemAffixes do
  use Ecto.Migration

  def change do
    create table(:socketable_item_affixes) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:item_type_id, references(:item_types, on_delete: :delete_all))
      add(:affix_id, references(:affixes, on_delete: :delete_all))
    end

    create(index(:socketable_item_affixes, [:item_id]))
    create(index(:socketable_item_affixes, [:item_type_id]))
    create(index(:socketable_item_affixes, [:affix_id]))
  end
end
