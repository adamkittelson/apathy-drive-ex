defmodule ApathyDrive.Repo.Migrations.RemoveEntitiesAbilities do
  use Ecto.Migration

  def change do
    drop(table(:entities_abilities))
  end
end
