defmodule ApathyDrive.Repo.Migrations.RemoveMinimumEssence do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      remove(:minimum_essence)
    end

    alter table(:mobiles) do
      remove(:minimum_essence)
    end
  end
end
