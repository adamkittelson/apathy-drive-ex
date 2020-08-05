defmodule ApathyDrive.Repo.Migrations.AddLearnedToCharacterAbilities do
  use Ecto.Migration

  def change do
    alter table(:characters_abilities) do
      add(:learned, :boolean, default: false)
    end
  end
end
