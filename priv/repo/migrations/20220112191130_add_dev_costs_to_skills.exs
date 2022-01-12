defmodule ApathyDrive.Repo.Migrations.AddDevCostsToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:dev_cost, :integer)
      add(:fast_dev_cost, :integer)
    end

    alter table(:characters_skills) do
      add(:current_level_times_trained, :integer)
    end
  end
end
