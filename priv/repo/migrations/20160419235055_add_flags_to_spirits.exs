defmodule ApathyDrive.Repo.Migrations.AddFlagsToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add :flags, :jsonb
    end
  end
end
