defmodule ApathyDrive.Repo.Migrations.ChangeMonsterAlignmentToInteger do
  use Ecto.Migration

  def up do
    alter table(:monsters) do
      modify :alignment, :integer
    end
  end

  def down do
    alter table(:monsters) do
      modify :alignment, :decimal
    end
  end
end
