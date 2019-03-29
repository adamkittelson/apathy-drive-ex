defmodule ApathyDrive.Repo.Migrations.RemoveManaFromMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      remove(:max_mana)
      remove(:mana_regen)
    end
  end
end
