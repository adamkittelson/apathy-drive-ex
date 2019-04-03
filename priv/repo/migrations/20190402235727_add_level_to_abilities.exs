defmodule ApathyDrive.Repo.Migrations.AddLevelToAbilities do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:level, :integer)
    end
  end
end
