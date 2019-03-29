defmodule ApathyDrive.Repo.Migrations.CreateEntitiesSpells do
  use Ecto.Migration

  def change do
    drop(table(:classes_spells))

    create table(:entities_spells) do
      add(:assoc_table, :string)
      add(:assoc_id, :integer)
      add(:spell_id, :integer)
      add(:level, :integer)

      timestamps
    end
  end
end
