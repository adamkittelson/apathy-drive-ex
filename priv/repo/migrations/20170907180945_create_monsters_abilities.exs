defmodule ApathyDrive.Repo.Migrations.CreateMonstersAbilities do
  use Ecto.Migration

  def change do
    create table(:monsters_abilities) do
      add(:monster_id, references(:monsters, on_delete: :delete_all))
      add(:ability_id, references(:abilities, on_delete: :delete_all))
      add(:value, :jsonb)
    end

    create(index(:monsters_abilities, [:monster_id, :ability_id], unique: true))
  end
end
