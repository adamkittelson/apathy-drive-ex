defmodule ApathyDrive.Repo.Migrations.UniqueAbilityCommandsAndNames do
  use Ecto.Migration

  def change do
    create(unique_index(:abilities, [:command]))
    drop(index(:abilities, [:name]))
    create(unique_index(:abilities, [:name]))
  end
end
