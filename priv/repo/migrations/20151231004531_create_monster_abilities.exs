defmodule ApathyDrive.Repo.Migrations.CreateMonsterAbilities do
  use Ecto.Migration

  def change do
    create table(:monster_abilities) do
      add(:monster_template_id, references(:monster_templates))
      add(:ability_id, references(:abilities))

      timestamps()
    end

    create(unique_index(:monster_abilities, [:monster_template_id, :ability_id]))
  end
end
