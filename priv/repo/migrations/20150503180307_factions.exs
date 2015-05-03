defmodule ApathyDrive.Repo.Migrations.Factions do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add :faction, :text
      remove :school
    end

    alter table(:spirits) do
      add :faction, :text
      remove :school
    end

    alter table(:rooms) do
      add :lair_faction, :text
    end
  end

end
