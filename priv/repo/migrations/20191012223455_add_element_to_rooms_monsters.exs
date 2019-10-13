defmodule ApathyDrive.Repo.Migrations.AddElementToRoomsMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add(:lore, :text)
    end
  end
end
