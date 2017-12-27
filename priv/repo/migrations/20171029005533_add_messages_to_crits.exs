defmodule ApathyDrive.Repo.Migrations.AddMessagesToCrits do
  use Ecto.Migration

  def change do
    alter table(:crits) do
      add :user_message, :text
      add :target_message, :text
      add :spectator_message, :text
    end
  end
end
