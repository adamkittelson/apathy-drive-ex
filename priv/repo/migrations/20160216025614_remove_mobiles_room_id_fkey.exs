defmodule ApathyDrive.Repo.Migrations.RemoveMonstersRoomIdFkey do
  use Ecto.Migration

  def change do
    Ecto.Adapters.SQL.query!(ApathyDrive.Repo, ~s(ALTER TABLE "public"."monsters" DROP CONSTRAINT "monsters_room_id_fkey";), [])
  end
end
