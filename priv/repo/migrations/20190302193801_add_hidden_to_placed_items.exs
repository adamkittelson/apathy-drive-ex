defmodule ApathyDrive.Repo.Migrations.AddHiddenToPlacedItems do
  use Ecto.Migration

  def change do
    alter table(:rooms_placed_items) do
      add(:hidden, :boolean, default: false)
    end
  end
end
