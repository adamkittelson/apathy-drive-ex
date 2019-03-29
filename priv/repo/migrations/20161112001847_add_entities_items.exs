defmodule ApathyDrive.Repo.Migrations.AddEntitiesItems do
  use Ecto.Migration

  def change do
    drop(table(:characters_items))
    drop(table(:rooms_items))

    create table(:entities_items) do
      add(:item_id, :integer)
      add(:assoc_table, :string)
      add(:assoc_id, :integer)
      add(:level, :integer)
      add(:equipped, :boolean)
      add(:strength, :integer)
      add(:intellect, :integer)
      add(:willpower, :integer)
      add(:agility, :integer)
      add(:health, :integer)
      add(:charm, :integer)

      timestamps
    end

    create(index(:entities_items, [:assoc_table, :assoc_id]))
  end
end
