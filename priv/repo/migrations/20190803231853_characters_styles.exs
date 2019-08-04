defmodule ApathyDrive.Repo.Migrations.CharactersStyles do
  use Ecto.Migration

  def change do
    create table(:characters_styles) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:item_id, references(:items, on_delete: :delete_all))
    end

    create(unique_index(:characters_styles, [:character_id, :item_id]))
  end
end
