defmodule ApathyDrive.Repo.Migrations.AddExperienceToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:experience, :integer)
    end
  end
end
