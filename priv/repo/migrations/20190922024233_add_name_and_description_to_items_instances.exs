defmodule ApathyDrive.Repo.Migrations.AddNameAndDescriptionToItemsInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:name, :text)
      add(:description, :text)
    end
  end
end
