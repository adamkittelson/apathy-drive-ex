defmodule ApathyDrive.Repo.Migrations.AddQualityToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:quality, :text)
    end
  end
end
