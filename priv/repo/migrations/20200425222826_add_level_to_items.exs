defmodule ApathyDrive.Repo.Migrations.AddLevelToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:level, :integer)
    end
  end
end
