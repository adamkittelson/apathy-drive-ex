defmodule ApathyDrive.Repo.Migrations.CreateMonsters do
  use Ecto.Migration

  def up do
    ["CREATE TABLE IF NOT EXISTS monsters( \
      id serial primary key, \
      monster_template_id integer, \
      room_id integer, \
      name text, \
      experience integer, \
      alignment decimal(8,2), \
      level integer, \
      skills jsonb, \
      limbs jsonb \
    )",
    "CREATE INDEX monsters_monster_template_id_index ON monsters (monster_template_id)",
    "CREATE INDEX monsters_room_id_index ON monsters (room_id)"]
  end

  def down do
    "DROP TABLE monsters"
  end
end
