defmodule ApathyDrive.Repo.Migrations.RemoveOldTables do
  use Ecto.Migration

  def change do
    drop(table(:classes))
    drop(table(:mobiles))
    drop(table(:monster_abilities))
    drop(table(:monster_templates))
    drop(table(:old_abilities))
    drop(table(:room_unities))
    drop(table(:spirit_item_recipes))
    drop(table(:spirits))
  end
end
