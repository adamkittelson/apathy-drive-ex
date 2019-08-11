defmodule ApathyDrive.Repo.Migrations.CreateCraftingRecipes do
  use Ecto.Migration

  def change do
    create table(:crafting_recipes) do
      add(:material_id, references(:materials, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:level, :integer)
      add(:type, :text)
      add(:armour_type, :text)
      add(:worn_on, :text)
      add(:weapon_type, :text)
      add(:weight, :integer)
      add(:speed, :integer)
      add(:cost_value, :integer)
      add(:cost_currency, :text)
      add(:material_amount, :integer)
    end

    create table(:crafting_recipes_traits) do
      add(:crafting_recipe_id, references(:crafting_recipes, on_delete: :delete_all))
      add(:trait_id, references(:traits, on_delete: :delete_all))
      add(:value, :jsonb)
    end

    alter table(:items_traits) do
      remove(:level, :integer)
    end

    drop(table(:materials_items))
  end
end
