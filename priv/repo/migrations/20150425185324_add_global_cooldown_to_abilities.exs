defmodule ApathyDrive.Repo.Migrations.AddGlobalCooldownToAbilities do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:global_cooldown, :float)
    end
  end
end
