defmodule ApathyDrive.Repo.Migrations.AddSkillToEnchantments do
  use Ecto.Migration

  def change do
    alter table(:enchantments) do
      add(:skill_id, references(:skills, on_delete: :delete_all))
    end
  end
end
