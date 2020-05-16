defmodule ApathyDrive.Repo.Migrations.AddAutoSneakToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:auto_sneak, :boolean, default: false)
    end
  end
end
