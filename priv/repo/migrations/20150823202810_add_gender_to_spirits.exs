defmodule ApathyDrive.Repo.Migrations.AddGenderToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add :gender, :text
    end
  end
end
