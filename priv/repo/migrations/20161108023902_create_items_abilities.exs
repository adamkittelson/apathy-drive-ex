defmodule ApathyDrive.Repo.Migrations.CreateItemsAbilities do
  use Ecto.Migration

  def change do
    alter table(:items) do
      remove(:abilities)
    end

    create table(:items_abilities) do
      add(:item_id, :integer)
      add(:ability_id, :integer)
      add(:value, :jsonb)

      timestamps()
    end
  end
end
