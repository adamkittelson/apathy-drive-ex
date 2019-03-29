defmodule ApathyDrive.Repo.Migrations.AddAttributesToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add(:strength, :integer)
      add(:agility, :integer)
      add(:will, :integer)
    end
  end
end
