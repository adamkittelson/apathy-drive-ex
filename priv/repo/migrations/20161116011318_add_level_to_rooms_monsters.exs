defmodule ApathyDrive.Repo.Migrations.AddLevelToRoomsMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add(:level, :integer)
    end
  end
end
