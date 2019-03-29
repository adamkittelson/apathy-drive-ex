defmodule ApathyDrive.Repo.Migrations.AddAreaNameUniqueIndex do
  use Ecto.Migration

  def change do
    create(index(:areas, [:name], unique: true))
  end
end
