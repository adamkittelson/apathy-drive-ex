defmodule ApathyDrive.Repo.Migrations.ChangeRoomAbilitiesToInteger do
  use Ecto.Migration

  def up do
    alter table(:rooms) do
      add :ability_id, :integer
      remove :room_ability
    end
  end

  def down do
    alter table(:rooms) do
      remove :ability_id
      add :room_ability, :string
    end
  end
end
