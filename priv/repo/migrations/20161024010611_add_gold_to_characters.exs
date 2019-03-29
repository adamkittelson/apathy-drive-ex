defmodule ApathyDrive.Repo.Migrations.AddGoldToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:gold, :bigint)
    end
  end
end
