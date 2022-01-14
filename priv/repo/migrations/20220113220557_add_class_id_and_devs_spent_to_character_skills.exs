defmodule ApathyDrive.Repo.Migrations.AddClassIdAndDevsSpentToCharacterSkills do
  use Ecto.Migration

  def change do
    alter table(:characters_skills) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:devs_spent, :integer)
    end
  end
end
