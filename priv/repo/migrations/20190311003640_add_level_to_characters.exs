defmodule ApathyDrive.Repo.Migrations.AddLevelToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:level, :integer, default: 1)
    end
  end
end
