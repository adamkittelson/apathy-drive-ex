defmodule ApathyDrive.Repo.Migrations.AddAutoToCharactersAbilities do
  use Ecto.Migration

  def change do
    alter table(:characters_abilities) do
      add(:auto, :boolean, default: false)
    end
  end
end
