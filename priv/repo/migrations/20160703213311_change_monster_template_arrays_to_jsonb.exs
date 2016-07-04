defmodule ApathyDrive.Repo.Migrations.ChangeMonsterTemplateArraysToJsonb do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      remove :adjectives
      remove :flags
      remove :unities

      add :adjectives, :jsonb
      add :flags, :jsonb
      add :unities, :jsonb
    end
  end

  def down do
    alter table(:monster_templates) do
      remove :adjectives
      remove :flags
      remove :unities

      add :adjectives, {:array, :string}
      add :flags, {:array, :string}
      add :unities, {:array, :string}
    end
  end
end
