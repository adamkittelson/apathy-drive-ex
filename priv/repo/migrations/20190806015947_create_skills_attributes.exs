defmodule ApathyDrive.Repo.Migrations.CreateSkillsAttributes do
  use Ecto.Migration

  def change do
    create table(:skills_attributes) do
      add(:skill_id, references(:skills, on_delete: :delete_all))
      add(:attribute_id, references(:attributes, on_delete: :delete_all))
    end
  end
end
