defmodule ApathyDrive.Repo.Migrations.AddTargetedToEmotes do
  use Ecto.Migration

  def change do
    alter table(:emotes) do
      add(:targeted, :boolean)
    end
  end
end
