defmodule ApathyDrive.Repo.Migrations.AddUsesToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:uses, :integer)
    end
  end
end
