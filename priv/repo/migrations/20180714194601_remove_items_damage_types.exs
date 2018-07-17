defmodule ApathyDrive.Repo.Migrations.RemoveItemsDamageTypes do
  use Ecto.Migration

  def change do
    drop(table(:items_damage_types))
  end
end
