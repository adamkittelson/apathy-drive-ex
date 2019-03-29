defmodule ApathyDrive.Repo.Migrations.CreateScripts do
  use Ecto.Migration

  def change do
    create table(:scripts) do
      add(:mongo_id, :text)
      add(:instructions, :jsonb)

      timestamps
    end

    create(unique_index(:scripts, [:mongo_id]))
  end
end
