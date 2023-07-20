defmodule ApathyDrive.Repo.Migrations.RemoveCostModifierFromTrainers do
  use Ecto.Migration

  def change do
    alter table(:trainers) do
      remove(:cost_modifier, :float)
    end
  end
end
