defmodule ApathyDrive.Repo.Migrations.AddHpRegenToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:hp_regen, :integer)
    end
  end
end
