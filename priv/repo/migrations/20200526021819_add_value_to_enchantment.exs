defmodule ApathyDrive.Repo.Migrations.AddValueToEnchantment do
  use Ecto.Migration

  def change do
    alter table(:enchantments) do
      add(:value, :jsonb)
    end
  end
end
