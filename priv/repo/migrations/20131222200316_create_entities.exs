defmodule ApathyDrive.Repo.Migrations.CreateEntities do
  use Ecto.Migration

  def up do
      "CREATE TABLE IF NOT EXISTS entities(id serial primary key, components json)"
    end

    def down do
      "DROP TABLE"
    end
end
