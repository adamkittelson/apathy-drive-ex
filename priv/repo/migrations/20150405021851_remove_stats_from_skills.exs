defmodule ApathyDrive.Repo.Migrations.RemoveStatsFromSkills do
  use Ecto.Migration

  def up do
    alter table(:skills) do
      remove :strength
      remove :agility
      remove :intelligence
      remove :health
    end
  end

  def down do
    alter table(:skills) do
      add :strength,         :integer
      add :agility,          :integer
      add :intelligence,     :integer
      add :health,           :integer
    end
  end
end
