defmodule ApathyDrive.Repo.Migrations.CreateCharactersAbilities do
  use Ecto.Migration

  def change do
    create table(:characters_abilities) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:ability_id, references(:abilities, on_delete: :delete_all))
      add(:passive, :boolean, default: false)
    end
  end
end
