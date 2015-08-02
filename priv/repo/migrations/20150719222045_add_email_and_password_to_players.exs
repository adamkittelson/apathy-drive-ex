defmodule ApathyDrive.Repo.Migrations.AddEmailAndPasswordToPlayers do
  use Ecto.Migration

  def change do
    alter table(:players) do
      add :email, :text
      add :password, :text
    end
  end
end
