defmodule ApathyDrive.Repo.Migrations.CreateRooms do
  use Ecto.Migration

  def up do
    ["CREATE TABLE IF NOT EXISTS rooms( \
      id serial primary key, \
      legacy_id text, \
      name text, \
      keywords text[], \
      description text, \
      light integer, \
      item_descriptions jsonb, \
      placed_items text[], \
      lair jsonb, \
      permanent_npc integer, \
      room_ability text, \
      start_room boolean, \
      shop_items text[], \
      trainable_skills text[], \
      exits jsonb, \
      created_at timestamp, \
      updated_at timestamp)",
    "CREATE INDEX rooms_legacy_id_index ON rooms (legacy_id)"]
  end

  def down do
    "DROP TABLE rooms"
  end

end
