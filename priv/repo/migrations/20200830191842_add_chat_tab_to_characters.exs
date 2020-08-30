defmodule ApathyDrive.Repo.Migrations.AddChatTabToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:chat_tab, :text, default: "all")
    end
  end
end
