defmodule ApathyDrive.Repo.Migrations.AddPermanentNpcToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add :permanent_npc, :integer
    end
  end
end
