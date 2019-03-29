defmodule ApathyDrive.Repo.Migrations.AdjustLairMonstersIndexes do
  use Ecto.Migration

  def change do
    execute("ALTER TABLE lair_monsters DROP CONSTRAINT lair_monsters_room_id_fkey")

    alter table(:lair_monsters) do
      modify(:room_id, references(:rooms, on_delete: :delete_all))
      modify(:monster_id, references(:monsters, on_delete: :delete_all))
    end
  end
end
