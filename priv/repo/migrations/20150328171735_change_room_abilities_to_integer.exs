defmodule ApathyDrive.Repo.Migrations.ChangeRoomAbilitiesToInteger do
  use Ecto.Migration

  def up do
    alter table(:rooms) do
      add :ability_id, :integer
      remove :room_ability
    end
  end

  def down do
    alter table(:item_templates) do
      add :room_ability, :text
      remove :ability_id
    end
  end
end
