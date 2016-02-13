defmodule ApathyDrive.Repo.Migrations.AddUnityToSpiritsAndMobiles do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      add :unity, :text
    end
    alter table(:spirits) do
      add :unity, :text
    end
  end
end
