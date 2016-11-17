defmodule ApathyDrive.Repo.Migrations.ChangeLairMonsterTemplateIdToMonsterId do
  use Ecto.Migration

  def change do
    alter table(:lair_monsters) do
      remove :monster_template_id
      add :monster_id, :integer
    end
  end
end
