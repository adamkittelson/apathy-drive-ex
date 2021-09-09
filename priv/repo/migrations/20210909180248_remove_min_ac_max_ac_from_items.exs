defmodule ApathyDrive.Repo.Migrations.RemoveMinAcMaxAcFromItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      remove(:min_ac)
      remove(:max_ac)
    end
  end
end
