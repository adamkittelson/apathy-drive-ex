defmodule ApathyDrive.Repo.Migrations.ItemAdjustments do
  use Ecto.Migration

  def change do
    alter table(:items) do
      remove :grade
      add :grade, :text
      remove :strength
      remove :agility
      remove :will
      add :global_drop, :boolean
    end
  end
end
