defmodule ApathyDrive.Repo.Migrations.AddOwnerIdToRoomsMonster do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      add(:owner_id, references(:characters, on_delete: :delete_all))
    end
  end
end
