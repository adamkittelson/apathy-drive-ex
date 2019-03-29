defmodule ApathyDrive.Repo.Migrations.RemoveUnusedColumnsFromCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:external_id)
      remove(:inventory)
      remove(:equipment)
    end
  end
end
