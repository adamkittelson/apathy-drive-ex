defmodule ApathyDrive.Repo.Migrations.CreateMonsterTemplates do
  use Ecto.Migration

  def up do
    ["CREATE TABLE IF NOT EXISTS monster_templates( \
      id serial primary key, \
      name text, \
      description text, \
      death_message text, \
      enter_message text, \
      exit_message text, \
      abilities integer[], \
      greeting text, \
      gender text, \
      game_limit integer, \
      adjectives text[], \
      strength integer, \
      agility integer, \
      intelligence integer, \
      health integer, \
      skills jsonb, \
      hit_verbs text[], \
      chance_to_follow integer, \
      damage jsonb, \
      disposition text, \
      alignment text, \
      possession_level integer, \
      limbs jsonb, \
      questions jsonb \
    )"]
  end

  def down do
    "DROP TABLE monster_templates"
  end
end
