defmodule ApathyDrive.Repo.Migrations.RemovePlayersCharactersRacesClasses do
  use Ecto.Migration

  def change do
    drop table(:players)
    drop table(:characters)
    drop table(:races)
    drop table(:classes)
  end
end
