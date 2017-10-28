defmodule ApathyDrive.Repo.Migrations.CreateRaceTraits do
  use Ecto.Migration

  def change do
    create table(:races_traits) do
      add :race_id, references(:races, on_delete: :delete_all)
      add :trait_id, references(:traits, on_delete: :delete_all)
      add :value, :jsonb
    end

    create index(:races_traits, [:race_id, :trait_id], unique: true)
  end
end
