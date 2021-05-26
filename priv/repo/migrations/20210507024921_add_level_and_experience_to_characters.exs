defmodule ApathyDrive.Repo.Migrations.AddLevelAndExperienceToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:experience, :bigint, default: 0)
    end
  end
end
