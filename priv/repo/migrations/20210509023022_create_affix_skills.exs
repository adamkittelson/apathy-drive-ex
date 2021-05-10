defmodule ApathyDrive.Repo.Migrations.CreateAffixSkills do
  use Ecto.Migration

  def change do
    create table(:affix_skills) do
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:affix_id, references(:affixes, on_delete: :delete_all))
      add(:description, :text)
      add(:value, :jsonb)
    end
  end
end
