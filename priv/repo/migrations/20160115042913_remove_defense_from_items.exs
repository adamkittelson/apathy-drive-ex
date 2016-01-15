defmodule ApathyDrive.Repo.Migrations.RemoveDefenseFromItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      remove :physical_defense
      remove :magical_defense
    end
  end
end
