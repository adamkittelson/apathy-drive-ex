defmodule ApathyDrive.Repo.Migrations.CreateSkillsSpells do
  use Ecto.Migration

  def change do
    create table(:skills_spells) do
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:spell_id, references(:spells, on_delete: :delete_all))
      add(:level, :integer)
    end
  end
end
