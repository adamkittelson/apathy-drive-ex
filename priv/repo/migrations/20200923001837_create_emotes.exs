defmodule ApathyDrive.Repo.Migrations.CreateEmotes do
  use Ecto.Migration

  def change do
    create table(:emotes) do
      add(:command, :text)
      add(:user_message, :text)
      add(:target_message, :text)
      add(:spectator_message, :text)
    end
  end
end
