defmodule ApathyDrive.Repo.Migrations.AddAutoPetCastingToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:auto_pet_casting, :boolean, default: true)
    end
  end
end
