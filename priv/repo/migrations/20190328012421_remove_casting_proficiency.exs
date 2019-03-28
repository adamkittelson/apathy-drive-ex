defmodule ApathyDrive.Repo.Migrations.RemoveCastingProficiency do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      remove(:casting_proficiency)
    end
  end
end
