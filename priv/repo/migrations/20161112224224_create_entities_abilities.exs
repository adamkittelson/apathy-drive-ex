defmodule ApathyDrive.Repo.Migrations.CreateEntitiesAbilities do
  use Ecto.Migration

  def change do
    drop(table(:items_abilities))
    drop(table(:spells_abilities))

    create table(:entities_abilities) do
      add(:assoc_table, :string)
      add(:assoc_id, :integer)
      add(:ability, :string)
      add(:value, :jsonb)

      timestamps
    end

    create(index(:entities_abilities, [:assoc_table, :assoc_id]))
  end
end
