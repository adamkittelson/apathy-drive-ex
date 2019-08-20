defmodule ApathyDrive.Repo.Migrations.AddMergeByToTraits do
  use Ecto.Migration

  def change do
    alter table(:traits) do
      add(:merge_by, :text)
    end
  end
end
