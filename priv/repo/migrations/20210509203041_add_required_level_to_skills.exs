defmodule ApathyDrive.Repo.Migrations.AddRequiredLevelToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:required_level, :integer)
    end

    create table(:items_instances_affix_skills) do
      add(:item_instance_id, references(:items_instances, on_delete: :delete_all))
      add(:affix_skill_id, references(:affix_skills, on_delete: :delete_all))

      add(:value, :jsonb)
      add(:description, :text)
    end
  end
end
