defmodule ApathyDrive.Repo.Migrations.RemoveMobilesRoomIdFkey do
  use Ecto.Migration

  def change do
    Ecto.Adapters.SQL.query!(ApathyDrive.Repo, ~s(ALTER TABLE "public"."mobiles" DROP CONSTRAINT "mobiles_room_id_fkey";), [])
  end
end
