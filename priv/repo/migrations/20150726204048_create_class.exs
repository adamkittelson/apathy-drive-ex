defmodule ApathyDrive.Repo.Migrations.CreateClass do
  use Ecto.Migration

  def change do
    create table(:classes) do
      add :name, :text
      add :description, :text
      add :properties, :jsonb

      timestamps
    end

  end
end
