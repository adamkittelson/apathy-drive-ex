defmodule ApathyDrive.Repo.Migrations.AddNameIndexToItems do
  use Ecto.Migration

  def change do
    create(index(:items, [:name]))
  end
end
