defmodule ApathyDrive.Repo.Migrations.CreateAbilityTraits do
  use Ecto.Migration

  def change do
    create table(:abilities_traits) do
      add :ability_id, references(:abilities, on_delete: :delete_all)
      add :trait_id, references(:traits, on_delete: :delete_all)
      add :value, :jsonb
    end

    create index(:abilities_traits, [:ability_id, :trait_id], unique: true)
  end
end
