defmodule ApathyDrive.Repo.Migrations.CreateCrits do
  use Ecto.Migration

  def change do
    create table(:crits) do
      add(:crit_table, :text)
      add(:letter, :text)
      add(:abilities, :jsonb)

      timestamps()
    end

    create(index(:crits, [:crit_table, :letter]))
  end
end
