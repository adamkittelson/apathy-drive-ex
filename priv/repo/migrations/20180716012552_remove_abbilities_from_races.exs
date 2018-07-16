defmodule ApathyDrive.Repo.Migrations.RemoveAbbilitiesFromRaces do
  use Ecto.Migration

  def change do
    alter table(:races) do
      remove(:abilities)
    end
  end
end
