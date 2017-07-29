defmodule ApathyDrive.Repo.Migrations.ChangeClassWeapons do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      add :weapon_type, :text
      add :weapon_hands, :text
      remove :weapon
    end
  end
end
