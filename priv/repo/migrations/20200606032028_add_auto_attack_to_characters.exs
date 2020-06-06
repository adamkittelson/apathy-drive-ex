defmodule ApathyDrive.Repo.Migrations.AddAutoAttackToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:auto_attack, :boolean, default: false)
    end
  end
end
