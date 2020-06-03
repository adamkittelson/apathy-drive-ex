defmodule ApathyDrive.Repo.Migrations.AddUniversalToSkills do
  use Ecto.Migration

  def change do
    alter table(:skills) do
      add(:universal, :boolean, default: false)
    end
  end
end
