defmodule ApathyDrive.Repo.Migrations.AddExpModifierToRacesAndClasses do
  use Ecto.Migration

  def change do
    alter table(:races) do
      add(:exp_modifier, :integer)
    end

    alter table(:classes) do
      add(:exp_modifier, :integer)
    end
  end
end
