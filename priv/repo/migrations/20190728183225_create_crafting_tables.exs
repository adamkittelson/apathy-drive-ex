defmodule ApathyDrive.Repo.Migrations.CreateCraftingTables do
  use Ecto.Migration

  def change do
    create table(:materials) do
      add(:name, :text)
    end

    create table(:characters_materials) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:material_id, references(:materials, on_delete: :delete_all))
    end

    create table(:materials_items) do
      add(:material_id, references(:materials, on_delete: :delete_all))
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:level, :integer)
      add(:amount, :integer)
    end

    alter table(:items_traits) do
      add(:level, :integer)
    end
  end
end
