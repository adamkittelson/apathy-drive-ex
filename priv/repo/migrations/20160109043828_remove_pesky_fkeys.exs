defmodule ApathyDrive.Repo.Migrations.RemovePeskyFkeys do
  use Ecto.Migration

  def change do
    Ecto.Adapters.SQL.query!(
      ApathyDrive.Repo,
      ~s(ALTER TABLE "public"."spirits" DROP CONSTRAINT "spirits_room_id_fkey";),
      []
    )

    Ecto.Adapters.SQL.query!(
      ApathyDrive.Repo,
      ~s(ALTER TABLE "public"."spirit_item_recipes" DROP CONSTRAINT "spirit_item_recipes_item_id_fkey";),
      []
    )
  end
end
