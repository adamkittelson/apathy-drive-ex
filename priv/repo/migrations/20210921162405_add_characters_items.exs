defmodule ApathyDrive.Repo.Migrations.AddCharactersItems do
  use Ecto.Migration

  def change do
    create table(:characters_items) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:count, :bigint, default: 0)
    end

    alter table(:items) do
      add(:stackable, :boolean, default: false)
    end

    create(index(:characters_items, [:character_id, :item_id], unique: true))
  end
end
