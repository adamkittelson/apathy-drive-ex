defmodule ApathyDrive.Repo.Migrations.CreateItemTemplates do
  use Ecto.Migration

  def up do
    ["CREATE TABLE IF NOT EXISTS item_templates( \
      id serial primary key, \
      name text, \
      keywords text[], \
      description text, \
      slot text, \
      worn_on jsonb, \
      hit_verbs text[], \
      damage jsonb, \
      required_skills jsonb, \
      speed decimal(8,2), \
      accuracy_skill text, \
      ac integer, \
      uses integer, \
      destruct_message text, \
      room_destruct_message text, \
      can_pick_up boolean, \
      value integer, \
      light integer, \
      light_duration integer, \
      always_lit boolean \
    )"]
  end

  def down do
    "DROP TABLE item_templates"
  end
end
