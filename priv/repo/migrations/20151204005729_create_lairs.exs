defmodule ApathyDrive.Repo.Migrations.CreateLairs do
  use Ecto.Migration

  def change do
    create table(:lair_monsters) do
      add(:room_id, references(:rooms))
      add(:monster_template_id, references(:monster_templates))

      timestamps
    end

    alter table(:rooms) do
      remove(:lair_monsters)
    end

    create(unique_index(:lair_monsters, [:room_id, :monster_template_id]))
  end
end
