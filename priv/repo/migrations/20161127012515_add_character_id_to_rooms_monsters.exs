defmodule ApathyDrive.Repo.Migrations.AddCharacterIdToRoomsMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add :character_id, :integer
    end

    create unique_index(:rooms_monsters, [:character_id])
  end
end
