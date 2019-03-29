defmodule ApathyDrive.Repo.Migrations.RenameSkillsSpells do
  use Ecto.Migration

  def change do
    rename(table(:skills_spells), to: table(:skills_abilities))
    rename(table(:skills_abilities), :spell_id, to: :ability_id)
  end
end
