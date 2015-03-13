defmodule ApathyDrive.Repo.Migrations.AddFlagsToAbilitiesAndMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:abilities) do
      add :flags, {:array, :string}
    end
    alter table(:monster_templates) do
      add :flags, {:array, :string}
    end
  end

  def down do
    alter table(:abilities) do
      remove :flags
    end
    alter table(:monster_templates) do
      remove :flags
    end
  end
end
