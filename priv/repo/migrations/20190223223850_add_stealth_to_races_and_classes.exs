defmodule ApathyDrive.Repo.Migrations.AddStealthToRacesAndClasses do
  use Ecto.Migration

  def change do
    alter table(:races) do
      add(:stealth, :boolean, default: false)
    end

    alter table(:classes) do
      add(:stealth, :boolean, default: false)
    end
  end
end
