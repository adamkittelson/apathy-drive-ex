defmodule ApathyDrive.Repo.Migrations.ChangePlacedItemsToIntArrayOnRooms do
  use Ecto.Migration

  def up do
    alter table(:rooms) do
      modify :placed_items, {:array, :integer}
    end
  end

  def down do
    alter table(:rooms) do
      modify :placed_items, {:array, :text}
    end
  end
end
