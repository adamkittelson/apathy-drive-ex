defmodule ApathyDrive.Repo.Migrations.AddAttacksPerRoundToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add :attacks_per_round, :integer
    end
  end
end
