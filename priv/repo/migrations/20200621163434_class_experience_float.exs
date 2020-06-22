defmodule ApathyDrive.Repo.Migrations.ClassExperienceFloat do
  use Ecto.Migration

  def change do
    alter table(:characters_classes) do
      modify(:experience, :float)
    end

    alter table(:characters) do
      remove(:experience)
    end
  end
end
