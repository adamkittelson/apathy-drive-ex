defmodule ApathyDrive.Repo.Migrations.RedoClasses do
  use Ecto.Migration

  def change do
    drop table(:classes)
    create table(:classes) do
      add :name, :text
      add :description, :text
      add :armour, :text
      add :weapon, :text
      add :abilities, :jsonb

      timestamps
    end

    alter table(:races) do
      add :abilities, :jsonb
    end
  end
end
