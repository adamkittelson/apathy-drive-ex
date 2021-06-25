defmodule ApathyDrive.Repo.Migrations.AddMaxLevelToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:max_level, :integer, default: 6)
    end
  end
end
