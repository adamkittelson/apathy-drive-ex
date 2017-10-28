defmodule ApathyDrive.Repo.Migrations.RemoveItemDrops do
  use Ecto.Migration

  def change do
    drop table(:item_drops)
  end
end
