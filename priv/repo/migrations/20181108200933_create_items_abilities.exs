defmodule ApathyDrive.Repo.Migrations.CreateItemsAbilities do
  use Ecto.Migration

  def change do
    create table(:item_ability_types) do
      # OnHit - ability that fires when hitting a monster with a weapon
      # Grant - ability useable if item is equipped
      # Learn - ability learned if the item is used (read)
      add(:name, :text)
    end

    create table(:items_abilities) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:ability_id, references(:abilities, on_delete: :delete_all))
      add(:type_id, references(:item_ability_types, on_delete: :delete_all))
    end
  end
end
