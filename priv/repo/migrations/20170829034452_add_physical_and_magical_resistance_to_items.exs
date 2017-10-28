defmodule ApathyDrive.Repo.Migrations.AddPhysicalAndMagicalResistanceToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add :physical_resistance, :integer
      add :magical_resistance, :integer
    end
  end
end
