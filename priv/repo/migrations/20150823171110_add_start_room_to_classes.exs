defmodule ApathyDrive.Repo.Migrations.AddStartRoomToClasses do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      add :start_room_id, :integer
    end
  end
end
