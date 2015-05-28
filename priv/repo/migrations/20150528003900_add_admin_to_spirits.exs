defmodule ApathyDrive.Repo.Migrations.AddAdminToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add :admin, :boolean, default: false
    end
  end
end
