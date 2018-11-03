defmodule ApathyDrive.Repo.Migrations.AddDeleteAtToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:delete_at, :utc_datetime)
    end
  end
end
