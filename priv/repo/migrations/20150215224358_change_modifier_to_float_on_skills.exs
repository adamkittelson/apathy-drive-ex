defmodule ApathyDrive.Repo.Migrations.ChangeDecimalsToFloats do
  use Ecto.Migration

  def up do
    alter table(:skills) do
      modify(:cost, :float)
    end
  end

  def down do
    alter table(:skills) do
      modify(:cost, :decimal)
    end
  end
end
