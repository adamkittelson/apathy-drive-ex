defmodule ApathyDrive.Repo.Migrations.AddNameToLimbs do
  use Ecto.Migration

  def change do
    alter table(:limbs) do
      add(:name, :text)
    end
  end
end
