defmodule ApathyDrive.Repo.Migrations.AddMovementToMonsterTemplatesAndMobiles do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      add :movement, :text
    end
    alter table(:monster_templates) do
      add :movement, :text
    end
  end
end
