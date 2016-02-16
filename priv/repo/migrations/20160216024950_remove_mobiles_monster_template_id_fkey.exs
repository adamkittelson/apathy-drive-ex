defmodule ApathyDrive.Repo.Migrations.RemoveMobilesMonsterTemplateIdFkey do
  use Ecto.Migration

  def change do
    Ecto.Adapters.SQL.query!(ApathyDrive.Repo, ~s(ALTER TABLE "public"."mobiles" DROP CONSTRAINT "mobiles_monster_template_id_fkey";), [])
  end
end
