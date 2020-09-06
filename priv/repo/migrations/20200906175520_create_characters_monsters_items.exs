defmodule ApathyDrive.Repo.Migrations.CreateCharactersMonstersItems do
  use Ecto.Migration

  def change do
    create table(:characters_monsters_items) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:monsters_items_id, references(:monsters_items, on_delete: :delete_all))
      add(:pity, :integer, default: 0)
    end

    create(index(:characters_monsters_items, :character_id))
    create(index(:characters_monsters_items, :monsters_items_id))
    create(index(:characters_monsters_items, [:character_id, :monsters_items_id]))
  end
end
