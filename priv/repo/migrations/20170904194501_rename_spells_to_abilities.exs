defmodule ApathyDrive.Repo.Migrations.RenameSpellsToAbilities do
  use Ecto.Migration

  def change do
    rename(table(:spells), to: table(:abilities))
  end
end
