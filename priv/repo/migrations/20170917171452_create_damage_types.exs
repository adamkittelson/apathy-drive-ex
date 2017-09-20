defmodule ApathyDrive.Repo.Migrations.CreateDamageTypes do
  use Ecto.Migration

  def change do
    create table(:damage_types) do
      add :name, :text
    end

    create table(:abilities_damage_types) do
      add :ability_id, references(:abilities, on_delete: :delete_all)
      add :damage_type_id, references(:damage_types, on_delete: :delete_all)
      add :kind, :text
      add :potency, :integer
    end

    create table(:characters_items) do
      add :character_id, references(:characters, on_delete: :delete_all)
      add :item_id, references(:items, on_delete: :delete_all)
      add :level, :integer
      add :equipped, :boolean, default: false
    end

    create table(:rooms_items) do
      add :room_id, references(:rooms, on_delete: :delete_all)
      add :item_id, references(:items, on_delete: :delete_all)
      add :level, :integer
      add :hidden, :boolean, default: false
    end

    drop table(:entities_items)
  end
end
