defmodule ApathyDrive.Repo.Migrations.CreateFactions do
  use Ecto.Migration

  def change do
    create table(:factions) do
      add :name, :text
      add :relationships, :jsonb

      timestamps
    end

    create index(:factions, ["lower(name)"], unique: true, name: :factions_lower_name_index)

    create table(:areas_factions) do
      add :area_id, :integer
      add :faction_id, :integer

      timestamps
    end

    create index(:areas_factions, [:area_id, :faction_id], unique: true)
  end
end
