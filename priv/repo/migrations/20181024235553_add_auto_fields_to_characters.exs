defmodule ApathyDrive.Repo.Migrations.AddAutoFieldsToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:auto_heal, :boolean)
      add(:auto_bless, :boolean)
      add(:auto_curse, :boolean)
      add(:auto_nuke, :boolean)
      add(:auto_roam, :boolean)
      add(:auto_flee, :boolean)
    end
  end
end
