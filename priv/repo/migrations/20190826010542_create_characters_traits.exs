defmodule ApathyDrive.Repo.Migrations.CreateCharactersTraits do
  use Ecto.Migration

  def change do
    create table(:characters_traits) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:trait_id, references(:traits, on_delete: :delete_all))
      add(:value, :jsonb)
    end

    create(index(:characters_traits, [:character_id, :trait_id], unique: true))
  end
end
