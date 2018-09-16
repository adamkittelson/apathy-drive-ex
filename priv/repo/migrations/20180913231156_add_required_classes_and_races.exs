defmodule ApathyDrive.Repo.Migrations.AddRequiredClassesAndRaces do
  use Ecto.Migration

  def change do
    create table(:items_classes) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:class_id, references(:classes, on_delete: :delete_all))
    end

    create(index(:items_classes, [:item_id, :class_id], unique: true))

    create table(:items_races) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:race_id, references(:races, on_delete: :delete_all))
    end

    create(index(:items_races, [:item_id, :race_id], unique: true))
  end
end
