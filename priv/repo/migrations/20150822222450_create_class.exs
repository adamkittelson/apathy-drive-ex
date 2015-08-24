defmodule ApathyDrive.Repo.Migrations.CreateClass do
  use Ecto.Migration

  def change do
    create table(:classes) do
      add :name, :text
      add :alignment, :text
      add :strength, :integer
      add :strength_per_level, :integer
      add :agility, :integer
      add :agility_per_level, :integer
      add :will, :integer
      add :will_per_level, :integer
      add :abilities, :jsonb

      timestamps
    end

  end
end
