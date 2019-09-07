defmodule ApathyDrive.Repo.Migrations.AddDecayToRoomMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add(:decay, :boolean)
    end
  end
end
