defmodule ApathyDrive.Repo.Migrations.RemoveEnchantmentUniqueConstraint do
  use Ecto.Migration

  def change do
    drop(index(:enchantments, [:items_instances_id, :ability_id], unique: true))
  end
end
