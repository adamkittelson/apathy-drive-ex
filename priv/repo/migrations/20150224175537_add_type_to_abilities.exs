defmodule ApathyDrive.Repo.Migrations.AddTypeToAbilities do
  use Ecto.Migration

  def up do
    alter table(:abilities) do
      add(:kind, :text)
    end
  end

  def down do
    alter table(:abilities) do
      remove(:kind)
    end
  end
end
