defmodule ApathyDrive.Repo.Migrations.RenameAbilitiesToTraits do
  use Ecto.Migration

  def change do
    rename(table(:abilities), to: table(:traits))
  end
end
