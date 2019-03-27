defmodule ApathyDrive.Repo.Migrations.CreateScrollReadingData do
  use Ecto.Migration

  def change do
    create table(:characters_abilities) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:ability_id, references(:abilities, on_delete: :delete_all))
    end

    alter table(:classes_abilities) do
      add(:auto_learn, :boolean, default: false)
    end

    create(index(:characters_abilities, [:character_id, :ability_id], unique: true))
    create(index(:characters_abilities, [:character_id]))
    create(index(:characters_abilities, [:ability_id]))
  end
end
