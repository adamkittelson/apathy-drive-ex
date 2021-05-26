defmodule ApathyDrive.Repo.Migrations.CreateSkillTags do
  use Ecto.Migration

  def change do
    create table(:tags) do
      add(:name, :text)
    end

    create table(:skill_tags) do
      add(:tag_id, references(:tags, on_delete: :delete_all))
      add(:skill_id, references(:skills, on_delete: :delete_all))
    end

    create table(:affix_tags) do
      add(:tag_id, references(:tags, on_delete: :delete_all))
      add(:affix_id, references(:affixes, on_delete: :delete_all))
      add(:value, :jsonb)
    end
  end
end
