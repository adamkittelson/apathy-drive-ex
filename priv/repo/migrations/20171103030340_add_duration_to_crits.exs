defmodule ApathyDrive.Repo.Migrations.AddDurationToCrits do
  use Ecto.Migration

  def change do
    alter table(:crits) do
      add :duration, :integer
    end
  end
end
