defmodule ApathyDrive.Repo.Migrations.AddExternalIdToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add :external_id, :text
    end
  end
end
