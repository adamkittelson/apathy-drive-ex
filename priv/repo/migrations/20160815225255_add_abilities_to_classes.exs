defmodule ApathyDrive.Repo.Migrations.AddAbilitiesToClasses do
  use Ecto.Migration

  def change do
    drop table(:class_abilities)

    alter table(:classes) do
      add :abilities, :jsonb
    end

  end
end
