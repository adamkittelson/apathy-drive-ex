defmodule ApathyDrive.Repo.Migrations.RemoveCharactersAbilities do
  use Ecto.Migration

  def change do
    drop(table(:characters_abilities))
  end
end
