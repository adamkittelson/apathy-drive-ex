defmodule ApathyDrive.Repo.Migrations.CreateAbilities do
  use Ecto.Migration

  def up do
    create table(:abilities) do
      add(:name, :text)
      add(:command, :text)
      add(:description, :text)
      add(:required_skills, :jsonb)
      add(:properties, :jsonb)

      timestamps
    end
  end

  def down do
    drop(table(:abilities))
  end
end
