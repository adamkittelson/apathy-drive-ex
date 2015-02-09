defmodule ApathyDrive.Repo.Migrations.CreateHints do
  use Ecto.Migration

  def up do
    create table(:hints) do
      add :name, :text
      add :body, :text

      timestamps
    end

    create index(:hints, [:name])
  end

  def down do
    drop table(:hints)
  end
end
