defmodule ApathyDrive.Repo.Migrations.AddExpAndLevelToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:experience, :bigint)
    end
  end
end
