defmodule ApathyDrive.Repo.Migrations.AddColumnsToTrainers do
  use Ecto.Migration

  def change do
    alter table(:trainers) do
      add(:type, :text)
      add(:level, :integer)
      add(:cost_modifier, :float)
    end
  end
end
