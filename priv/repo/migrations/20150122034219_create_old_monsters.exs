defmodule ApathyDrive.Repo.Migrations.CreateOldMonsters do
  use Ecto.Migration

  def up do
    create table(:monsters) do
      add(:monster_template_id, references(:monster_templates))
      add(:room_id, references(:rooms))
      add(:name, :text)
      add(:experience, :integer)
      add(:alignment, :decimal)
      add(:level, :integer)
      add(:skills, :jsonb)
      add(:limbs, :jsonb)

      timestamps
    end

    create(index(:monsters, [:monster_template_id]))
    create(index(:monsters, [:room_id]))
  end

  def down do
    drop(table(:monsters))
  end
end
