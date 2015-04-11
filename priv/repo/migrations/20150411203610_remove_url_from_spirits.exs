defmodule ApathyDrive.Repo.Migrations.RemoveUrlFromSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      remove :url
    end
  end
end
