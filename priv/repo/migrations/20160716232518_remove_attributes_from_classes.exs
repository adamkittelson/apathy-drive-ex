defmodule ApathyDrive.Repo.Migrations.RemoveAttributesFromClasses do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      remove(:strength)
      remove(:strength_per_level)
      remove(:agility)
      remove(:agility_per_level)
      remove(:will)
      remove(:will_per_level)
    end
  end
end
