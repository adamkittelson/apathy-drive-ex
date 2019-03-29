defmodule ApathyDrive.Repo.Migrations.CreatePlayer do
  use Ecto.Migration

  def change do
    create table(:players) do
      add(:external_id, :string)
      add(:admin, :boolean, default: false)

      timestamps
    end
  end
end
