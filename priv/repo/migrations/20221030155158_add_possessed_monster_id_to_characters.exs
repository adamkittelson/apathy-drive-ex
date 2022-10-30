defmodule ApathyDrive.Repo.Migrations.AddPossessedMonsterIdToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:possessed_monster_id, references(:rooms_monsters))
    end

    alter table(:rooms_monsters) do
      add(:possessing_character_id, references(:characters))
      remove(:character_id)
    end
  end
end
