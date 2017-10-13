defmodule ApathyDrive.Repo.Migrations.AddPurchasedToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add :purchased, :boolean
    end
  end
end
