defmodule ApathyDrive.Repo.Migrations.AddAutoRestToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:auto_rest, :boolean, default: false)
    end
  end
end
