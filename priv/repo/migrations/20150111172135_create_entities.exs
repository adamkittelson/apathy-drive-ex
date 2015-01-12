defmodule ApathyDrive.Repo.Migrations.CreateEntities do
  use Ecto.Migration

  def up do
    "CREATE TABLE IF NOT EXISTS entities( \
     id serial primary key, \
     components jsonb)"
  end

  def down do
    "DROP TABLE entities"
  end

end
