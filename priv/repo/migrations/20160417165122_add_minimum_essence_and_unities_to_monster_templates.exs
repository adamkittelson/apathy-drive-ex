defmodule ApathyDrive.Repo.Migrations.AddMinimumEssenceAndUnitiesToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add :unities, {:array, :string}
      add :minimum_essence, :bigint
    end

    alter table(:mobiles) do
      add :minimum_essence, :bigint
    end
  end
end
