defmodule ApathyDrive.Repo.Migrations.AddExpMultiplierToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:exp_multiplier, :float)
    end
  end
end
