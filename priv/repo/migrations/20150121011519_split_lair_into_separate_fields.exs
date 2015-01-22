defmodule ApathyDrive.Repo.Migrations.SplitLairIntoSeparateFields do
  use Ecto.Migration

  def up do
    ["ALTER TABLE rooms ADD COLUMN lair_size integer",
     "ALTER TABLE rooms ADD COLUMN lair_monsters integer[]",
     "ALTER TABLE rooms ADD COLUMN lair_frequency integer",
     "ALTER TABLE rooms DROP COLUMN lair"]
  end

  def down do
    ["ALTER TABLE rooms DROP COLUMN lair_size",
     "ALTER TABLE rooms DROP COLUMN lair_monsters",
     "ALTER TABLE rooms DROP COLUMN lair_frequency",
     "ALTER TABLE rooms ADD COLUMN lair jsonb"]
  end
end
