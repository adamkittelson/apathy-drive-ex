defmodule ApathyDrive.Repo.Migrations.CreateItems do
  use Ecto.Migration

  def up do
    create table(:items) do
      add :item_template_id,    references(:item_templates)
      add :room_id,             references(:rooms)
      add :monster_id,          references(:monsters)

      timestamps
    end

    create index(:items, [:item_template_id])
    create index(:items, [:room_id])
    create index(:items, [:monster_id])
  end

  def down do
    drop table(:items)
  end
end
