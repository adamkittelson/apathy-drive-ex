defmodule ApathyDrive.Repo.Migrations.AddQualityLevelToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:quality_level, :integer)
    end
  end
end
