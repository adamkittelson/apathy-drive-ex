defmodule ApathyDrive.Repo.Migrations.CreateMonsterTraits do
  use Ecto.Migration

  def change do
    create table(:monsters_traits) do
      add :monster_id, references(:monsters)
      add :trait_id, references(:traits)
      add :value, :jsonb
    end

    create index(:monsters_traits, [:monster_id, :trait_id], unique: true)
  end
end
