defmodule ApathyDrive.Repo.Migrations.AddCastingProficiencyToClasses do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      add(:casting_proficiency, :integer)
    end

    alter table(:abilities) do
      add(:difficulty, :integer)
    end
  end
end
