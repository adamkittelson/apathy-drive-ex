defmodule ApathyDrive.Repo.Migrations.CreateHints do
  use Ecto.Migration

  def up do
    ["CREATE TABLE IF NOT EXISTS hints( \
      id serial primary key, \
      name text, \
      body text)",
    "CREATE INDEX hints_name_index ON hints (name)"]
  end

  def down do
    "DROP TABLE hints"
  end
end
