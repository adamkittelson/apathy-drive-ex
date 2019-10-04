defmodule ApathyDrive.Repo.Migrations.RemoveDecay do
  use Ecto.Migration

  def change do
    alter table(:rooms_monsters) do
      remove(:decay)
      add(:delete_at, :utc_datetime)
    end
  end
end
