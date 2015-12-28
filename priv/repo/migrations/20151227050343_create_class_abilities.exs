defmodule ApathyDrive.Repo.Migrations.CreateClassAbilities do
  use Ecto.Migration

  def change do
    create table(:class_abilities) do
      add :class_id,   references(:classes)
      add :ability_id, references(:abilities)
      add :level, :integer

      timestamps
    end

    alter table(:classes) do
      remove :abilities
    end

    create unique_index(:class_abilities, [:class_id, :ability_id])
  end
end
