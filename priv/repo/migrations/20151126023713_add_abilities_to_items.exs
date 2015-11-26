defmodule ApathyDrive.Repo.Migrations.AddAbilitiesToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add :abilities, :jsonb
    end
  end
end
