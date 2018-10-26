defmodule ApathyDrive.Repo.Migrations.CreateChannelHistory do
  use Ecto.Migration

  def change do
    create table(:channel_history) do
      add(:character_id, references(:characters))
      add(:character_name, :text)
      add(:channel_name, :text)
      add(:game_name, :text)
      add(:message, :text)

      timestamps()
    end
  end
end
