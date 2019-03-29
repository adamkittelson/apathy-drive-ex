defmodule ApathyDrive.Repo.Migrations.ChangeClassUnitiesToJsonb do
  use Ecto.Migration

  def up do
    alter table(:classes) do
      remove(:unities)
      add(:unities, :jsonb)
    end
  end

  def down do
    alter table(:classes) do
      remove(:unities)
      add(:unities, {:array, :string})
    end
  end
end
