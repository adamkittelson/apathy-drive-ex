defmodule ApathyDrive.Repo.Migrations.RemoveRequiredStrAndRequiredAgiFromItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      remove(:required_str)
      remove(:required_agi)
    end
  end
end
