defmodule ApathyDrive.Repo.Migrations.AddControllerToRoomUnities do
  use Ecto.Migration

  def change do
    alter table(:room_unities) do
      add :controlled_by, :text
    end

    create index(:room_unities, [:controlled_by])

  end
end
