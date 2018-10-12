defmodule ApathyDrive.Repo.Migrations.AddGameLimitToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:game_limit, :integer)
    end
  end
end
