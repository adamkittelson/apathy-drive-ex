defmodule ApathyDrive.Repo.Migrations.CreateMonstersItems do
  use Ecto.Migration

  def change do
    create table(:monsters_items) do
      add(:monster_id, references(:monsters, on_delete: :delete_all))
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:chance, :integer)
    end
  end
end
