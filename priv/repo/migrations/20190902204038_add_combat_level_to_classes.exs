defmodule ApathyDrive.Repo.Migrations.AddCombatLevelToClasses do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      add(:combat_level, :integer)
    end
  end
end
