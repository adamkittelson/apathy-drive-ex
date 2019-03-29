defmodule ApathyDrive.Repo.Migrations.AddUniqueNameIndexToAbilities do
  use Ecto.Migration

  def change do
    create(index(:abilities, [:name], unique: true))
  end
end
