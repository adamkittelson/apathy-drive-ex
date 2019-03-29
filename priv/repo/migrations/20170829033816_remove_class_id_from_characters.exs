defmodule ApathyDrive.Repo.Migrations.RemoveClassIdFromCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:class_id)
    end
  end
end
