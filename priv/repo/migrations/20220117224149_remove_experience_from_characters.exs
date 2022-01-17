defmodule ApathyDrive.Repo.Migrations.RemoveExperienceFromCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:experience)
    end
  end
end
