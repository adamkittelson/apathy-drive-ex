defmodule ApathyDrive.Repo.Migrations.RemoveUniqueIndexOnAbilityNames do
  use Ecto.Migration

  def change do
    drop(index(:abilities, [:name], unique: true))
  end
end
