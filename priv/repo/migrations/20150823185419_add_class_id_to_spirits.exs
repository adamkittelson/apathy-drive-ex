defmodule ApathyDrive.Repo.Migrations.AddClassIdToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      remove :faction
      remove :alignment
      add :class_id, :integer
    end
  end
end
