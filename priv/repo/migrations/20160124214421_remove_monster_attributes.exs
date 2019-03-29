defmodule ApathyDrive.Repo.Migrations.RemoveMonsterAttributes do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      remove(:strength)
      remove(:agility)
      remove(:will)
    end

    drop(table(:monsters))
  end
end
