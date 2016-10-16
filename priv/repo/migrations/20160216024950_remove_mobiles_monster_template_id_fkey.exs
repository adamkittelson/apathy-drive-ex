defmodule ApathyDrive.Repo.Migrations.RemoveMonstersMonsterTemplateIdFkey do
  use Ecto.Migration

  def change do
    Ecto.Adapters.SQL.query!(ApathyDrive.Repo, ~s(ALTER TABLE "public"."monsters" DROP CONSTRAINT "monsters_monster_template_id_fkey";), [])
  end
end
