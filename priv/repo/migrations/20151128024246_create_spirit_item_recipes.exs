defmodule ApathyDrive.Repo.Migrations.CreateSpiritItemRecipes do
  use Ecto.Migration

  def change do
    create table(:spirit_item_recipes) do
      add :spirit_id, references(:spirits)
      add :item_id,   references(:items)

      timestamps
    end

    create unique_index(:spirit_item_recipes, [:spirit_id, :item_id])
  end
end
