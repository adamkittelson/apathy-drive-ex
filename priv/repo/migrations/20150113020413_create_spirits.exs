defmodule ApathyDrive.Repo.Migrations.CreateSpirits do
  use Ecto.Migration

  def up do
    "CREATE TABLE IF NOT EXISTS spirits( \
      id serial primary key, \
      name text, \
      experience integer, \
      level integer, \
      url text, \
      hints jsonb, \
      room_id integer, \
      created_at timestamp, \
      updated_at timestamp)"
  end

  def down do
    "DROP TABLE spirits"
  end
end
