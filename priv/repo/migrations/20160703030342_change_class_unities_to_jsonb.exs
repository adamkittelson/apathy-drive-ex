defmodule ApathyDrive.Repo.Migrations.ChangeClassUnitiesToJsonb do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      remove :unities
      add :unities, :jsonb
    end
  end
end
