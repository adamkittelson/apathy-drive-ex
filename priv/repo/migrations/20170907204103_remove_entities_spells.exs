defmodule ApathyDrive.Repo.Migrations.RemoveEntitiesSpells do
  use Ecto.Migration

  def change do
    drop(table(:entities_spells))
  end
end
