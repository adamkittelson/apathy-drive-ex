defmodule ApathyDrive.Repo.Migrations.AddTypeToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:type, :text)
    end

    alter table(:trainers) do
      remove(:type)
    end
  end
end
