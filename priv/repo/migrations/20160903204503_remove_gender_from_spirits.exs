defmodule ApathyDrive.Repo.Migrations.RemoveGenderFromSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      remove :gender
    end
  end
end
