defmodule ApathyDrive.Repo.Migrations.AddItemsDamageTypes do
  use Ecto.Migration

  def change do
    create table(:items_damage_types) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:damage_type_id, references(:damage_types, on_delete: :delete_all))
      add(:kind, :text)
      add(:potency, :integer)
    end

    create table(:items_instances) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:level, :integer)
      add(:equipped, :boolean, default: false)
      add(:hidden, :boolean, default: false)
    end

    drop(table(:rooms_items))
    drop(table(:characters_items))
  end
end
