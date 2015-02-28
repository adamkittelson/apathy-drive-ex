defmodule ApathyDrive.Repo.Migrations.AddEquippedToItems do
  use Ecto.Migration

  def up do
    alter table(:items) do
      add :equipped, :boolean
    end
  end

  def down do
    alter table(:items) do
      remove :equipped
    end
  end
end
