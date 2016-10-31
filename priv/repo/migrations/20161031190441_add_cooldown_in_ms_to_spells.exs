defmodule ApathyDrive.Repo.Migrations.AddCooldownInMsToSpells do
  use Ecto.Migration

  def change do
    alter table(:spells) do
      add :cooldown_in_ms, :integer
    end
  end
end
