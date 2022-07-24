defmodule ApathyDrive.Repo.Migrations.AddWelcomeTokenToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:welcome_token, :text)
      add(:email_verified, :boolean, default: false)
    end
  end
end
