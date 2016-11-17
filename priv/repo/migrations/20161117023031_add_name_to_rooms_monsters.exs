defmodule ApathyDrive.Repo.Migrations.AddNameToRoomsMonsters do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add :name, :string
    end
  end
end
