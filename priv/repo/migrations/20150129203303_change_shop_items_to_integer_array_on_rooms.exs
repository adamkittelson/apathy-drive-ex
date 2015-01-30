defmodule ApathyDrive.Repo.Migrations.ChangeShopItemsToIntegerArrayOnRooms do
  use Ecto.Migration

  def up do
    "ALTER TABLE rooms ALTER COLUMN shop_items TYPE integer[] using NULL"
  end

  def down do
    "ALTER TABLE rooms ALTER COLUMN shop_items TYPE text[] using NULL"
  end
end
