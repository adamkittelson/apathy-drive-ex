defmodule ApathyDrive.Repo.Migrations.CreateShopItems do
  use Ecto.Migration

  def change do
    create table(:shop_items) do
      add :room_id, references(:rooms)
      add :item_id, references(:items)

      timestamps
    end

    create unique_index(:shop_items, [:room_id, :item_id])
  end
end
