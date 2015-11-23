defmodule ApathyDrive.Repo.Migrations.CreateItemDrops do
  use Ecto.Migration

  def change do
    create table(:item_drops) do
      add :monster_id, references(:monster_templates)
      add :item_id,    references(:items)
      add :chance,     :integer

      timestamps
    end

    create unique_index(:item_drops, [:monster_id, :item_id])
  end
end
