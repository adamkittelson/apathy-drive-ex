defmodule ApathyDrive.Repo.Migrations.AddGettableToItemsInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:getable, :boolean)
    end
  end
end
