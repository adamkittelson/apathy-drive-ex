defmodule ApathyDrive.Repo.Migrations.AddUsesToItems do
  use Ecto.Migration

  def up do
    alter table(:items) do
      add :uses, :integer
    end
  end

  def down do
    alter table(:items) do
      remove :uses
    end
  end
end
