defmodule ApathyDrive.Repo.Migrations.AddInventoryAndEquipmentToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add(:inventory, :jsonb)
      add(:equipment, :jsonb)
    end
  end
end
