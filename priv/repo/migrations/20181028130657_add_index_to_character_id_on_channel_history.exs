defmodule ApathyDrive.Repo.Migrations.AddIndexToCharacterIdOnChannelHistory do
  use Ecto.Migration

  def change do
    create(index(:channel_history, :character_id))
  end
end
