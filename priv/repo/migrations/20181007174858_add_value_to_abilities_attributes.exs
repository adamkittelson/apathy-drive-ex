defmodule ApathyDrive.Repo.Migrations.AddValueToAbilitiesAttributes do
  use Ecto.Migration

  def change do
    alter table(:abilities_attributes) do
      add(:value, :integer)
    end
  end
end
