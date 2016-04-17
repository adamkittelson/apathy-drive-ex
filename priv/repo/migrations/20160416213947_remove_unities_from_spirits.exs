defmodule ApathyDrive.Repo.Migrations.RemoveUnitiesFromSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      remove :unities
    end
  end
end
