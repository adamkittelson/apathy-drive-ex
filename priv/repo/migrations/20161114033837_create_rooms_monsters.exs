defmodule ApathyDrive.Repo.Migrations.CreateRoomsMonsters do
  use Ecto.Migration

  def change do
    create table(:rooms_monsters, primary_key: false) do
      add :id, :bigserial, primary_key: true
      add :room_id, :integer
      add :monster_id, :integer
      add :strength, :integer
      add :agility, :integer
      add :intellect, :integer
      add :willpower, :integer
      add :health, :integer
      add :charm, :integer

      timestamps
    end
  end
end
