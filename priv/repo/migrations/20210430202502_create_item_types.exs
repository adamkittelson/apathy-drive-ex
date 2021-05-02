defmodule ApathyDrive.Repo.Migrations.CreateItemTypes do
  use Ecto.Migration

  def change do
    create table(:item_types) do
      add(:name, :text)
    end

    create table(:items_item_types) do
      add(:item_id, references(:items, on_delete: :delete_all))
    end

    create table(:affixes_items_types) do
      add(:affix_id, references(:affixes, on_delete: :delete_all))
      add(:item_type_id, references(:item_types, on_delete: :delete_all))

      add(:allowed, :boolean)
    end
  end
end
