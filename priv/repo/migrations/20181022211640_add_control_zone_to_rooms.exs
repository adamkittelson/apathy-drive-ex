defmodule ApathyDrive.Repo.Migrations.AddControlZoneToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add(:zone_monster_limit, :integer)
      add(:zone_controller_id, references(:rooms))
    end
  end
end
